#lang racket
(require "simpleParser.rkt")
(require "lex.rkt")
(provide interpret)

;the main interpret function
(define interpret
  (lambda (filename)
    (caadr                    ;result will be first value of second list
     (parse_statement_list    ;begin parsing - returns final state
      (parser filename)
      (list '(RETURN) '(()))))))

;highest level: parse statement-list
(define parse_statement_list
  (lambda (statement_list state)
    (cond
      ((null? statement_list) state)   ;NOTE: is this the correct termination for empty statement-list? (I think so...)
      ((list? (car statement_list)) (parse_statement_list
                                      (cdr statement_list)
                                      (parse_statement (car statement_list) state))) 
      (else
       (error "parse_statement_list: Invalid parse - Expected a list, but got" (car statement_list))))))  ;error on invalid parse should never be thrown

;check which type of statement: parse statement
; new parse_statement function
(define parse_statement
  (lambda (statement state)
    (cond
      ((number? statement) statement)
      ((boolean? statement) statement)                  ; Convert boolean to our representation
      ((is_boolean? statement) (boolean_to_racket statement))                                 ; Return the boolean if it is a boolean
      ((not (pair? statement)) (lookup statement state))                 ; Check if the variable has been assigned a value
      ((null? statement) state)
      ((is_reserved_word? statement) (eval_reserved_word statement state))
      ((is_math_expr? statement) (eval_math_expr statement state))
      ((is_boolean_expr? statement) (eval_boolean_expr statement state))
      (else (error 'parse_statement (format "Unknown statement: ~a" statement))))))

; parse_statement helper functions

; Function to identify the type of statement
(define identifier car)

; Function to parse reserved words
(define eval_reserved_word
  (lambda (statement state)
    (cond
      ((eq? (identifier statement) 'var) (declare statement state))
      ((eq? (identifier statement) '=) (assign statement state))
      ((eq? (identifier statement) 'return) (return statement state))
      ((eq? (identifier statement) 'if) (interpret_if statement state))
      ((eq? (identifier statement) 'while) (interpret_while statement state))
      (else (error 'eval_reserved_word (format "How in the world did this error happen??? Statement: ~a" statement))))))

; Function to check if the statement is a reserved word
(define is_reserved_word?
  (lambda (statement)
    (or
      (eq? (identifier statement) 'var)
      (eq? (identifier statement) 'return)
      (eq? (identifier statement) '=)
      (eq? (identifier statement) 'if)
      (eq? (identifier statement) 'while))))

; function to check if the statement is a math expression
(define is_math_expr?
  (lambda (statement)
    (or
      (eq? (identifier statement) '+)
      (eq? (identifier statement) '-)
      (eq? (identifier statement) '*)
      (eq? (identifier statement) '/)
      (eq? (identifier statement) '%))))

; Function to check if the statement is a comparison
(define is_comparison?
(lambda (statement)
    (or
      (eq? (identifier statement) '>)
      (eq? (identifier statement) '<)
      (eq? (identifier statement) '<=)
      (eq? (identifier statement) '>=)
      (eq? (identifier statement) '==)
      (eq? (identifier statement) '!=))))

; Function to check if the statement is a boolean
(define is_boolean?
  (lambda (statement)
    (or
      (eq? statement 'true)
      (eq? statement 'false))))

; Function to check if the statement is a boolean expression
(define is_boolean_expr?
  (lambda (statement)
    (or
      (is_comparison? statement)
      (eq? (identifier statement) '&&)
      (eq? (identifier statement) '||)
      (eq? (identifier statement) '!))))

(define boolean_to_racket
  (lambda (statement)
    (if (eq? statement 'true)
      #t
      #f)))
     
(define boolean_to_defined
  (lambda (statement)
    (if statement
      'true
      'false)))

;--------------statements-------------------------

;declaration statement
(define declare
  (lambda (statement state)
    (if (null? (cddr statement)) ; if there is no value assigned to the variable
        (empty_declare statement state)
        (value_declare statement state))))

; Helper functions for declare

; key and value of the statement
(define state_key cadr)
(define state_value caddr)

; empty declaration
(define empty_declare
  (lambda (statement state)
    (bind (state_key statement) '() state)))

; declaration with a value
(define value_declare
  (lambda (statement state)
    (bind (state_key statement) (parse_statement (state_value statement) state) state)))

;assignment statement
(define assign
  (lambda (statement state)
    (if (does_exist? (state_key statement) state)
      (update (state_key statement) (parse_statement (state_value statement) state) state)
      (error 'assign "variable not declared" (statement)))))
    

;return statement
(define return
  (lambda (statement state)
    (update
      'RETURN
      (parse_statement (cadr statement) state)
      state)))

; conditionals helpers
(define condition cadr)
(define body caddr)
(define else_body cadddr)

;if statement
(define interpret_if
  (lambda (statement state)
    (if (null? (cdddr statement)) ; if there is no else statement
      (if_without_else statement state)
      (if_with_else statement state))))

; Helpers to separate an if with an else and a if without an else
(define if_with_else
  (lambda (statement state)
    (if (parse_statement (condition statement) state)
      (parse_statement (body statement) state)
      (parse_statement (else_body statement) state))))

(define if_without_else
  (lambda (statement state)
    (if (parse_statement (condition statement) state)
      (parse_statement (body statement) state)
      state)))

;while statement
(define interpret_while
  (lambda (statement state)
    (if (parse_statement (condition statement) state)
      (interpret_while statement (parse_statement (body statement) state))
      state)))


;----------------bindings -------------


; function to just roll through the state: get the cdr of both the lists in state
(define next_in_state
  (lambda (state)
    (list (cdar state) (cdadr state))))

; return a boolean on whether a variable name exists in the state
(define does_exist?
  (lambda (name state)
    (cond
      ((null? (car state)) #f)
      ((eq? name (caar state)) #t)
      (else (does_exist? name (next_in_state state))))))

;create binding
; bind a name to a value and return the updated state
(define bind
  (lambda (name value state)
    (if (does_exist? name state)
        (error 'bind (format "Variable ~a has already been declared" name))
        (list (append (car state) (list name)) (append (cadr state) (list value))))))

;lookup binding
; return the value assigned to a name in the state
; Errors if the variable does not exist or has not been assigned a value
(define lookup
  (lambda (name state)
    (cond
      ((not (does_exist? name state)) (error 'lookup (format "Variable ~a has not been declared" name))) ;error messages not updated
      ((eq? name (caar state)) (caadr state))
      ((eq? name (caar state)) (if (null? (caadr state))
                                (error 'lookup (format "Variable ~a has not been assigned a value" name))
                                (caadr state)))
      (else (lookup name (next_in_state state))))))

;update binding
; cps form of update
(define update-cps
  (lambda (name value state return)
    (if (not (does_exist? name state))
        (error 'update-cps (format "Variable ~a has not been declared" name))
        (cond
          ((null? (car state)) (return '() '()))
          ((eq? name (caar state)) (return (car state) (cons value (cdadr state))))
          (else (update-cps name value (next_in_state state) (lambda (v1 v2) (return (cons (caar state) v1) (cons (caadr state) v2)))))))))

; make it so you just run update instead of update-cps
(define update
  (lambda (name value state)
    (update-cps name value state (lambda (v1 v2) (list v1 v2)))))


;---------------values-------------

; Helper functions for eval_math_expr and eval_boolean_expr
(define operator car)
(define firstoperand cadr)
(define secondoperand caddr)

; Evaluate the math expression
(define eval_math_expr
  (lambda (expr state)
    (cond
      ((eq? '+ (operator expr)) (+ (parse_statement (firstoperand expr) state) (parse_statement (secondoperand expr) state)))
      ((eq? '- (operator expr)) (if (null? (cddr expr)) ; if its unary sign or subtraction
                                        (- 0 (parse_statement (firstoperand expr) state))
                                        (- (parse_statement (firstoperand expr) state) (parse_statement (secondoperand expr) state))))
      ((eq? '* (operator expr)) (* (parse_statement (firstoperand expr) state) (parse_statement (secondoperand expr) state)))
      ((eq? '/ (operator expr)) (quotient (parse_statement (firstoperand expr) state) (parse_statement (secondoperand expr) state)))
      ((eq? '% (operator expr)) (remainder (parse_statement (firstoperand expr) state) (parse_statement (secondoperand expr) state)))
      (else (error 'eval_math_expr (format "Invalid operator: ~a" expr))))))

; Evaluate the boolean expression
(define eval_boolean_expr
  (lambda (expr state)
    (cond
      ((eq? '> (operator expr)) (> (parse_statement (firstoperand expr) state) (parse_statement (secondoperand expr) state)))
      ((eq? '< (operator expr)) (< (parse_statement (firstoperand expr) state) (parse_statement (secondoperand expr) state)))
      ((eq? '<= (operator expr)) (<= (parse_statement (firstoperand expr) state) (parse_statement (secondoperand expr) state)))
      ((eq? '>= (operator expr)) (>= (parse_statement (firstoperand expr) state) (parse_statement (secondoperand expr) state)))
      ((eq? '== (operator expr)) (eq? (parse_statement (firstoperand expr) state) (parse_statement (secondoperand expr) state)))
      ((eq? '!= (operator expr)) (eq? (parse_statement (firstoperand expr) state) (parse_statement (secondoperand expr) state)))
      ((eq? '|| (operator expr)) (or (parse_statement (firstoperand expr) state) (parse_statement (secondoperand expr) state)))
      ((eq? '&& (operator expr)) (and (parse_statement (firstoperand expr) state) (parse_statement (secondoperand expr) state)))
      ((eq? '! (operator expr)) (not (parse_statement (firstoperand expr) state)))
      (else (error 'eval_boolean_expr (format "Invalid operator: ~a" expr))))))

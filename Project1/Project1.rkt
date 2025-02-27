#lang racket
(require "simpleParser.rkt")
(require "lex.rkt")
(provide interpret)

;the main interpret function
(define interpret
  (lambda (filename)
    (caadr                    ;result will be first value of second list
     (interpret_statement_list    ;begin parsing - returns final state
      (parser filename)
      (list '(RETURN) '(()))))))

;highest level: parse statement-list
(define interpret_statement_list
  (lambda (statement_list state)
    (cond
      ((null? statement_list) state)   ;NOTE: is this the correct termination for empty statement-list? (I think so...)
      ((list? (car statement_list)) (interpret_statement_list
                                      (cdr statement_list)
                                      (SV_interpret_statement (car statement_list) state))) 
      (else
       (error "interpret_statement_list: Invalid parse - Expected a list, but got" (car statement_list))))))  ;error on invalid parse should never be thrown

;check which type of statement: parse statement
; new interpret_statement function
(define SV_interpret_statement
  (lambda (statement state)
    (cond
      ((number? statement) statement)
      ((boolean? statement) (V_boolean_to_atomic statement))                  ; Convert boolean to our representation
      ((V_is_boolean? statement) statement)                                    ; Return the boolean if it is a boolean
      ((not (pair? statement)) (S_lookup statement state))                     ; Check if the variable has been assigned a value
      ((null? statement) state)
      ((V_is_reserved_word? statement) (S_eval_reserved_word statement state))
      ((V_is_math_expr? statement) (V_eval_math_expr statement state))
      ((V_is_boolean_expr? statement) (V_eval_boolean_expr statement state))
      (else (error 'SV_interpret_statement (format "Unknown statement: ~a" statement))))))

; SV_interpret_statement helper functions

; Function to identify the type of statement
(define identifier car)

; Function to parse reserved words
(define S_eval_reserved_word
  (lambda (statement state)
    (cond
      ((eq? (identifier statement) 'var) (S_declare statement state))
      ((eq? (identifier statement) '=) (S_assign statement state))
      ((eq? (identifier statement) 'return) (S_return statement state))
      ((eq? (identifier statement) 'if) (S_interpret_if statement state))
      ((eq? (identifier statement) 'while) (S_interpret_while statement state))
      (else (error 'S_eval_reserved_word (format "How in the world did this error happen??? Statement: ~a" statement))))))

; Function to check if the statement is a reserved word
(define V_is_reserved_word?
  (lambda (statement)
    (or
      (eq? (identifier statement) 'var)
      (eq? (identifier statement) 'return)
      (eq? (identifier statement) '=)
      (eq? (identifier statement) 'if)
      (eq? (identifier statement) 'while))))

; function to check if the statement is a math expression
(define V_is_math_expr?
  (lambda (statement)
    (or
      (eq? (identifier statement) '+)
      (eq? (identifier statement) '-)
      (eq? (identifier statement) '*)
      (eq? (identifier statement) '/)
      (eq? (identifier statement) '%))))

; Function to check if the statement is a comparison
(define V_is_comparison?
  (lambda (statement)
    (or
      (eq? (identifier statement) '>)
      (eq? (identifier statement) '<)
      (eq? (identifier statement) '<=)
      (eq? (identifier statement) '>=)
      (eq? (identifier statement) '==)
      (eq? (identifier statement) '!=))))

; Function to check if the statement is a boolean
(define V_is_boolean?
  (lambda (statement)
    (or
      (eq? statement 'true)
      (eq? statement 'false))))

; Function to check if the statement is a boolean expression
(define V_is_boolean_expr?
  (lambda (statement)
    (or
      (V_is_comparison? statement)
      (eq? (identifier statement) '&&)
      (eq? (identifier statement) '||)
      (eq? (identifier statement) '!))))

; Function to check if the statement is a boolean
(define V_is_true?
  (lambda (statement)
    (if (eq? statement 'true)
      #t
      #f)))
     
; Function to convert boolean to defined
(define V_boolean_to_atomic
  (lambda (statement)
    (if statement
      'true
      'false)))

;--------------statements-------------------------

;declaration statement
(define S_declare
  (lambda (statement state)
    (if (null? (cddr statement)) ; if there is no value assigned to the variable
        (S_empty_declare statement state)
        (S_value_declare statement state))))

; Helper functions for S_declare

; key and value of the statement
(define V_state_key cadr)
(define V_state_value caddr)

; empty declaration
(define S_empty_declare
  (lambda (statement state)
    (S_bind (V_state_key statement) '() state)))

; declaration with a value
(define S_value_declare
  (lambda (statement state)
    (S_bind (V_state_key statement) (SV_interpret_statement (V_state_value statement) state) state)))

;assignment statement
(define S_assign
  (lambda (statement state)
    (if (V_does_exist? (V_state_key statement) state)
      (S_update (V_state_key statement) (SV_interpret_statement (V_state_value statement) state) state)
      (error 'S_assign "variable not declared" (statement)))))
    

;return statement
(define S_return
  (lambda (statement state)
    (S_update
      'RETURN
      (SV_interpret_statement (cadr statement) state)
      state)))

; conditionals helpers
(define V_condition cadr)
(define V_body caddr)
(define V_else_body cadddr)

;if statement
(define S_interpret_if
  (lambda (statement state)
    (if (null? (cdddr statement)) ; if there is no else statement
      (S_if_without_else statement state)
      (S_if_with_else statement state))))

; Helpers to separate an if with an else and a if without an else
; if with else
(define S_if_with_else
  (lambda (statement state)
    (if (V_is_true? (SV_interpret_statement (V_condition statement) state))
      (SV_interpret_statement (V_body statement) state)
      (SV_interpret_statement (V_else_body statement) state))))

; if without else
(define S_if_without_else
  (lambda (statement state)
    (if (V_is_true? (SV_interpret_statement (V_condition statement) state))
      (SV_interpret_statement (V_body statement) state)
      state)))

;while statement
(define S_interpret_while
  (lambda (statement state)
    (if (V_is_true? (SV_interpret_statement (V_condition statement) state))
      (S_interpret_while statement (SV_interpret_statement (V_body statement) state))
      state)))


;----------------bindings -------------


; function to just roll through the state: get the cdr of both the lists in state
(define S_next_in_state
  (lambda (state)
    (list (cdar state) (cdadr state))))

; return a boolean on whether a variable name exists in the state
(define V_does_exist?
  (lambda (name state)
    (cond
      ((null? (car state)) #f)
      ((eq? name (caar state)) #t)
      (else (V_does_exist? name (S_next_in_state state))))))

;create binding
; bind a name to a value and return the updated state
(define S_bind
  (lambda (name value state)
    (if (V_does_exist? name state)
        (error 'bind (format "Variable ~a has already been declared" name))
        (list (append (car state) (list name)) (append (cadr state) (list value))))))

;lookup binding
; return the value assigned to a name in the state
; Errors if the variable does not exist or has not been assigned a value
(define S_lookup
  (lambda (name state)
    (cond
      ((not (V_does_exist? name state)) (error 'S_lookup (format "Variable ~a has not been declared" name)))
      ((eq? name (caar state)) (caadr state))
      ((eq? name (caar state)) (if (null? (caadr state))
                                (error 'S_lookup (format "Variable ~a has not been assigned a value" name))
                                (caadr state)))
      (else (S_lookup name (S_next_in_state state))))))

;update binding
; cps form of update
(define S_update_cps
  (lambda (name value state return)
    (if (not (V_does_exist? name state))
        (error 'S_update_cps (format "Variable ~a has not been declared" name))
        (cond
          ((null? (car state)) (return '() '()))
          ((eq? name (caar state)) (return (car state) (cons value (cdadr state))))
          (else (S_update_cps name value (S_next_in_state state) (lambda (v1 v2) (return (cons (caar state) v1) (cons (caadr state) v2)))))))))

; make it so you just run update instead of S_update_cps
(define S_update
  (lambda (name value state)
    (S_update_cps name value state (lambda (v1 v2) (list v1 v2)))))


;---------------values-------------

; Helper functions for eval_math_expr and eval_boolean_expr
(define V_operator car)
(define V_first_operand cadr)
(define V_second_operand caddr)

; Evaluate the math expression
(define V_eval_math_expr
  (lambda (expr state)
    (cond
      ((eq? '+ (V_operator expr)) (+
                                    (SV_interpret_statement (V_first_operand expr) state)
                                    (SV_interpret_statement (V_second_operand expr) state)))
      ((eq? '- (V_operator expr)) (if (null? (cddr expr)) ; if its unary sign or subtraction
                                        (- 0 (SV_interpret_statement (V_first_operand expr) state))
                                        (- (SV_interpret_statement (V_first_operand expr) state) (SV_interpret_statement (V_second_operand expr) state))))
      ((eq? '* (V_operator expr)) (*
                                    (SV_interpret_statement (V_first_operand expr) state)
                                    (SV_interpret_statement (V_second_operand expr) state)))
      ((eq? '/ (V_operator expr)) (quotient
                                    (SV_interpret_statement (V_first_operand expr) state)
                                    (SV_interpret_statement (V_second_operand expr) state)))
      ((eq? '% (V_operator expr)) (remainder
                                    (SV_interpret_statement (V_first_operand expr) state)
                                    (SV_interpret_statement (V_second_operand expr) state)))
      (else (error 'V_eval_math_expr (format "Invalid operator: ~a" expr))))))

; Evaluate the boolean expression
(define V_eval_boolean_expr
  (lambda (expr state)
    (cond
      ((eq? '> (V_operator expr)) (V_boolean_to_atomic (>
                                                         (SV_interpret_statement (V_first_operand expr) state)
                                                         (SV_interpret_statement (V_second_operand expr) state))))
      ((eq? '< (V_operator expr)) (V_boolean_to_atomic (<
                                                         (SV_interpret_statement (V_first_operand expr) state)
                                                         (SV_interpret_statement (V_second_operand expr) state))))
      ((eq? '<= (V_operator expr)) (V_boolean_to_atomic (<=
                                                          (SV_interpret_statement (V_first_operand expr) state)
                                                          (SV_interpret_statement (V_second_operand expr) state))))
      ((eq? '>= (V_operator expr)) (V_boolean_to_atomic (>=
                                                          (SV_interpret_statement (V_first_operand expr) state)
                                                          (SV_interpret_statement (V_second_operand expr) state))))
      ((eq? '== (V_operator expr)) (V_boolean_to_atomic (eq?
                                                          (SV_interpret_statement (V_first_operand expr) state)
                                                          (SV_interpret_statement (V_second_operand expr) state))))
      ((eq? '!= (V_operator expr)) (V_boolean_to_atomic (not (eq?
                                                               (SV_interpret_statement (V_first_operand expr) state)
                                                               (SV_interpret_statement (V_second_operand expr) state)))))
      ((eq? '|| (V_operator expr)) (V_boolean_to_atomic (or
                                                       (V_is_true? (SV_interpret_statement (V_first_operand expr) state))
                                                       (V_is_true? (SV_interpret_statement (V_second_operand expr) state)))))
      ((eq? '&& (V_operator expr)) (V_boolean_to_atomic (and
                                                       (V_is_true? (SV_interpret_statement (V_first_operand expr) state))
                                                       (V_is_true? (SV_interpret_statement (V_second_operand expr) state)))))
      ((eq? '! (V_operator expr)) (V_boolean_to_atomic (not (V_is_true? (SV_interpret_statement (V_first_operand expr) state)))))
      (else (error 'V_eval_boolean_expr (format "Invalid operator: ~a" expr))))))

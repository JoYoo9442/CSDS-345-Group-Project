#lang racket
(require "simpleParser.rkt")
(require "lex.rkt")

;; We made a custom tester file, so this is just for that
(provide interpret)

;; -----------------Interpretation----------------- ;;

;the main interpret function
(define interpret
  (lambda (filename)
    (interpret_statement    ; begin parsing - returns final state
      (list '() '())        ; Initial state
      (parser filename)
      (lambda (s v) v))))

; Function to identify the type of statement
(define identifier car)

;; Check which type of statement and return state/value
(define interpret_statement
  (lambda (state statement return)
    (if (list? statement)
      (cond
        [(null? statement) (return state '())] ; End of program reached with no valid return
        [(is_reserved_word? statement) (eval_reserved_word state statement return)]
        [(is_math_statement? statement) (eval_integer state statement return)]
        [(is_boolean_statement? statement) (eval_boolean state statement return)]
        [(null? (cdr statement)) (interpret_statement state (car statement) return)]
        [else (interpret_statement ; Statement is a list of statements
                state
                (car statement)
                (lambda (s v)
                  (interpret_statement s (cdr statement) return)))])
      (cond
        [(number? statement) (return state statement)]
        [(boolean? statement) (return state (boolean_to_atomic statement))]  ; Convert boolean to our representation
        [(is_boolean? statement) (return state statement)]                   ; Return the boolean if it is a boolean
        [else (lookup state statement return)]))))                           ; Check if the variable has been assigned a value

(define get_value
  (lambda (s v)
    v))

;; ----interpret_statement helper functions------- ;;

; Function to parse reserved words
(define eval_reserved_word
  (lambda (state statement return)
    (cond
      [(eq? (identifier statement) 'var) (declare state statement return)]
      [(eq? (identifier statement) '=) (assign state statement return)]
      [(eq? (identifier statement) 'return) (atom_return state statement return)]
      [(eq? (identifier statement) 'if) (interpret_if state statement return)]
      [(eq? (identifier statement) 'while) (interpret_while state statement return)]
      [else (error 'eval_reserved_word (format "How in the world did this error happen??? Statement: ~a" statement))])))

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
(define is_math_statement?
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
(define is_boolean_statement?
  (lambda (statement)
    (or
      (is_comparison? statement)
      (eq? (identifier statement) '&&)
      (eq? (identifier statement) '||)
      (eq? (identifier statement) '!))))

; Function to check if the statement is a boolean
(define is_true?
  (lambda (value)
    (if (eq? value 'true)
      #t
      #f)))
     
; Function to convert boolean to defined
(define boolean_to_atomic
  (lambda (statement)
    (if statement
      'true
      'false)))

;----------------bindings -------------
; function to just roll through the state: get the cdr of both the lists in state
(define next_in_state
  (lambda (state)
    (list (cdar state) (cdadr state))))

; Function to attach the car of the lists in state to a future state
(define preserve_state
  (lambda (state new_state)
    (list (cons (caar state) (car new_state)) (cons (caadr state) (cadr new_state)))))

; Function to query whether a variable has been declared
(define is_declared?
  (lambda (state name)
    (cond
      [(null? (car state)) #f]
      [(eq? name (caar state)) #t]
      [else (is_declared? (next_in_state state) name)])))

;create binding
; bind a name to a value and return the updated state
(define bind
  (lambda (state name value return)
    (if (is_declared? state name)
      (error 'bind (format "Variable ~a has already been declared" name))
      (interpret_statement state value
                           (lambda (s v)
                             (return (list (cons name (car s)) (cons v (cadr s))) v))))))

; lookup binding
; return the value assigned to a name in the state
; Errors if the variable does not exist or has not been assigned a value
(define lookup
  (lambda (state name return)
    (cond
      [(null? (car state)) (error 'lookup (format "Variable ~a has not been declared" name))]
      [(eq? name (caar state)) (if (null? (caadr state))
                                 (error 'lookup (format "Variable ~a has not been assigned a value" name))
                                 (return state (caadr state)))]
      [else (lookup (next_in_state state) name
                    (lambda (s v) (return (preserve_state state s) v)))])))

;update binding
; cps form of update
(define update
  (lambda (state name value return)
    (cond
      [(null? (car state)) (error 'update (format "Variable ~a has not been declared" name))]
      [(eq? name (caar state)) (return (list (car state) (cons value (cdadr state))) value)]
      [else (update (next_in_state state) name value
                    (lambda (s v) (return (preserve_state state s) v)))])))

;--------------statements-------------------------

; Helper functions for declare
; key and value of the statement
(define key cadr)
(define value cddr)

;declaration statement
(define declare
  (lambda (state statement return)
    (bind state (key statement) (value statement) return)))

; Assiignment statement
(define assign
  (lambda (state statement return)
    (interpret_statement state (value statement)
                         (lambda (s v)
                           (update s (key statement) v return)))))

;return statement
(define atom_return
  (lambda (state statement return)
    (interpret_statement state (cadr statement) return)))

; conditionals helpers
(define condition cadr)
(define body caddr)
(define else_body cdddr)

;if statement
(define interpret_if
  (lambda (state statement return)
    (interpret_statement state (condition statement)
                                 (lambda (s v)
                                   (if (is_true? v)
                                     (interpret_statement s (body statement) return)
                                     (interpret_statement s (else_body statement) return))))))

;while statement
(define interpret_while
  (lambda (state statement return)
    (interpret_statement state (condition statement)
                         (lambda (s1 v1)
                           (if (is_true? v1)
                             (interpret_statement s1 (body statement) (lambda (s2 v2)
                                                                        (interpret_while s2 statement return)))
                             (return s1 '()))))))

;---------------values-------------

; Helper functions for eval_integer and eval_boolean
(define operator car)
(define first_operand cadr)
(define second_operand caddr)

; Evaluate the math expression
(define eval_integer
  (lambda (state statement return)
    (cond
      [(eq? '+ (operator statement)) (interpret_statement state (first_operand statement)
                                                          (lambda (s1 v1)
                                                            (interpret_statement s1 (second_operand statement)
                                                                                (lambda (s2 v2)
                                                                                  (return s2 (+ v1 v2))))))]
      [(eq? '- (operator statement)) (if (null? (cddr statement))
                                                (interpret_statement state (first_operand statement)
                                                                    (lambda (s v)
                                                                      (return s (- 0 v))))
                                                (interpret_statement state (first_operand statement)
                                                                     (lambda (s1 v1)
                                                                       (interpret_statement s1 (second_operand statement)
                                                                                            (lambda (s2 v2)
                                                                                              (return s2 (- v1 v2)))))))]
      [(eq? '* (operator statement)) (interpret_statement state (first_operand statement)
                                                          (lambda (s1 v1)
                                                            (interpret_statement s1 (second_operand statement)
                                                                                (lambda (s2 v2)
                                                                                  (return s2 (* v1 v2))))))]
      [(eq? '/ (operator statement)) (interpret_statement state (first_operand statement)
                                                          (lambda (s1 v1)
                                                            (interpret_statement s1 (second_operand statement)
                                                                                (lambda (s2 v2)
                                                                                  (return s2 (quotient v1 v2))))))]
      [(eq? '% (operator statement)) (interpret_statement state (first_operand statement)
                                                          (lambda (s1 v1)
                                                            (interpret_statement s1 (second_operand statement)
                                                                                (lambda (s2 v2)
                                                                                  (return s2 (remainder v1 v2))))))]
      [else (error 'eval_integer (format "Invalid operator: ~a" statement))])))

; Evaluate the boolean expression
(define eval_boolean
  (lambda (state statement return)
    (cond
      [(eq? '> (operator statement)) (interpret_statement state (first_operand statement)
                                                          (lambda (s1 v1)
                                                            (interpret_statement s1 (second_operand statement)
                                                                                (lambda (s2 v2)
                                                                                  (return s2 (boolean_to_atomic (> v1 v2)))))))]
      [(eq? '< (operator statement)) (interpret_statement state (first_operand statement)
                                                          (lambda (s1 v1)
                                                            (interpret_statement s1 (second_operand statement)
                                                                                (lambda (s2 v2)
                                                                                  (return s2 (boolean_to_atomic (< v1 v2)))))))]
      [(eq? '>= (operator statement)) (interpret_statement state (first_operand statement)
                                                          (lambda (s1 v1)
                                                            (interpret_statement s1 (second_operand statement)
                                                                                (lambda (s2 v2)
                                                                                  (return s2 (boolean_to_atomic (>= v1 v2)))))))]
      [(eq? '<= (operator statement)) (interpret_statement state (first_operand statement)
                                                          (lambda (s1 v1)
                                                            (interpret_statement s1 (second_operand statement)
                                                                                (lambda (s2 v2)
                                                                                  (return s2 (boolean_to_atomic (<= v1 v2)))))))]
      [(eq? '== (operator statement)) (interpret_statement state (first_operand statement)
                                                          (lambda (s1 v1)
                                                            (interpret_statement s1 (second_operand statement)
                                                                                (lambda (s2 v2)
                                                                                  (return s2 (boolean_to_atomic (eq? v1 v2)))))))]
      [(eq? '!= (operator statement)) (interpret_statement state (first_operand statement)
                                                          (lambda (s1 v1)
                                                            (interpret_statement s1 (second_operand statement)
                                                                                (lambda (s2 v2)
                                                                                  (return s2 (boolean_to_atomic (not (eq? v1 v2))))))))]
      [(eq? '|| (operator statement)) (interpret_statement state (first_operand statement)
                                                          (lambda (s1 v1)
                                                            (interpret_statement s1 (second_operand statement)
                                                                                (lambda (s2 v2)
                                                                                  (return s2 (boolean_to_atomic
                                                                                               (or
                                                                                                 (is_true? v1)
                                                                                                 (is_true? v2))))))))]
      [(eq? '&& (operator statement)) (interpret_statement state (first_operand statement)
                                                          (lambda (s1 v1)
                                                            (interpret_statement s1 (second_operand statement)
                                                                                (lambda (s2 v2)
                                                                                  (return s2 (boolean_to_atomic
                                                                                               (and
                                                                                                 (is_true? v1)
                                                                                                 (is_true? v2))))))))]
      [(eq? '! (operator statement)) (interpret_statement state (first_operand statement)
                                                          (lambda (s v)
                                                            (return s (boolean_to_atomic
                                                                        (not (is_true? v))))))]
      [else (error 'eval_boolean (format "Invalid operator: ~a" statement))])))

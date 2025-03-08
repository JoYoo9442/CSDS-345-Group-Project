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
      (list '(() ()))        ; Initial state begins with the initial layer
      (parser filename)
      (lambda (s v) s v))))

;; Check which type of statement and return state/value
(define interpret_statement
  (lambda (state statement return)
    (if (list? statement)
      (cond
        [(null? statement) (return state '())] ; End of program reached with no valid return
        [(null? (cdr statement)) (interpret_statement state (car statement) return)]
        [(is_math_statement? statement) (eval_integer state statement return)]
        [(is_boolean_statement? statement) (eval_boolean state statement return)]
        [(is_reserved_word? statement) (eval_reserved_word state statement return)]
        [(is_goto? (caar statement)) (eval_goto state (caar statement) return)]
        [else (interpret_statement ; Statement is a list of statements
                state
                (car statement)
                (lambda (s v)
                  (interpret_statement s (cdr statement) return)))])
      (cond
        [(number? statement) (return state statement)]
        [(boolean? statement) (return state (boolean_to_atomic statement))]  ; Convert boolean to our representation
        [(is_boolean? statement) (return state statement)]                   ; Return the boolean if it is a boolean
        [(is_goto? statement) (eval_goto state statement return)]
        [else (lookup state statement return)]))))                           ; Check if the variable has been assigned a value

;; ----interpret_statement helper functions------- ;;

; Function to identify the type of statement
(define identifier car)

; Function to parse reserved words
(define eval_reserved_word
  (lambda (state statement return)
    (cond
      [(eq? (identifier statement) 'var) (declare state statement return)]
      [(eq? (identifier statement) '=) (assign state statement return)]
      [(eq? (identifier statement) 'return) (atom_return state statement)] ; Don't need return here since we skip rest
      [(eq? (identifier statement) 'if) (interpret_if state statement return)]
      [(eq? (identifier statement) 'while) (interpret_while state statement return)]
      [(eq? (identifier statement) 'begin) (begin_block state statement return)]
      [else (error 'eval_reserved_word (format "How in the world did this error happen??? Statement: ~a" statement))])))

; Function to check if the statement is a reserved word
(define is_reserved_word?
  (lambda (statement)
    (or (eq? (identifier statement) 'var)
        (eq? (identifier statement) 'return)
        (eq? (identifier statement) '=)
        (eq? (identifier statement) 'if)
        (eq? (identifier statement) 'while)
        (eq? (identifier statement) 'begin))))

(define is_goto?
  (lambda (statement)
    (or (eq? statement 'continue)
        (eq? statement 'break))))

(define base_state?
  (lambda (state)
    (null? (next_layers state))))

(define eval_goto
  (lambda (state statement return)
    (cond
      [(base_state? state) (error 'eval_goto "Cannot use goto statement outside of a block")]
      [(eq? statement 'continue) (return state (box 'continue))]
      [(eq? statement 'break) (return state (box 'break))]
      [else (error 'eval_goto (format "Invalid goto: ~a" statement))])))

; function to check if the statement is a math expression
(define is_math_statement?
  (lambda (statement)
    (or (eq? (identifier statement) '+)
        (eq? (identifier statement) '-)
        (eq? (identifier statement) '*)
        (eq? (identifier statement) '/)
        (eq? (identifier statement) '%))))

; Function to check if the statement is a comparison
(define is_comparison?
  (lambda (statement)
    (or (eq? (identifier statement) '>)
        (eq? (identifier statement) '<)
        (eq? (identifier statement) '<=)
        (eq? (identifier statement) '>=)
        (eq? (identifier statement) '==)
        (eq? (identifier statement) '!=))))

; Function to check if the statement is a boolean
(define is_boolean?
  (lambda (statement)
    (or (eq? statement 'true)
        (eq? statement 'false))))

; Function to check if the statement is a boolean expression
(define is_boolean_statement?
  (lambda (statement)
    (or (is_comparison? statement)
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
; Functions to make the rest of the functions read more like English
(define top_layer car)
(define next_layers cdr)

; These next two functions assume passing in just a layer of the state
(define layer_keys car)
(define layer_values cadr)

; function to just roll through the top layer get the cdr of both the lists in state
; Will roll over to the next layer if the top layer is gone through
(define next_in_state
  (lambda (state)
    (cond
      [(null? state) state]
      [(null? (layer_keys (top_layer state))) (next_layers state)]
      [else (cons
              (list
                (cdr (layer_keys (top_layer state)))
                (cdr (layer_values (top_layer state))))
              (next_layers state))])))

; Function to attach the car of the lists in state to a future state
(define preserve_state
  (lambda (state new_state)
    (cond
      [(null? state) new_state]
      [(null? (layer_keys (top_layer state))) (cons (top_layer state) new_state)]
      [else (cons
              (list
                (cons
                  (car (layer_keys (top_layer state)))
                  (layer_keys (top_layer new_state)))
                (cons
                  (car (layer_values (top_layer state)))
                  (layer_values (top_layer new_state))))
              (next_layers new_state))])))

; Function to query whether a variable has been declared
; Can take in states
(define is_declared?
  (lambda (state name)
    (cond
      [(null? state) #f]
      [(null? (layer_keys (top_layer state))) (is_declared? (next_layers state) name)]
      [(eq? name (car (layer_keys (top_layer state)))) #t]
      [else (is_declared? (next_in_state state) name)])))

; Function to cons a name and value to the top layer of the state
; Takes in a state
(define cons_to_state
  (lambda (state name value)
    (cons
      (list
        (cons name (layer_keys (top_layer state)))
        (cons value (layer_values (top_layer state))))
      (next_layers state))))

; Bind a name to a value and return the updated state
; Allows duplicate declarations as long as they are in different layers
(define bind
  (lambda (state name value return)
    (if (is_declared? (list (top_layer state)) name) ; Only check top layer for declaration (Make a "new state")
      (error 'bind (format "Variable ~a has already been declared" name))
      (interpret_statement state value
                           (lambda (s v)
                             (return (cons_to_state s name v) v))))))

; lookup binding
; return the value assigned to a name in the state
; Errors if the variable does not exist or has not been assigned a value
(define lookup
  (lambda (state name return)
    (cond
      [(null? state) (error 'lookup (format "Variable ~a has not been declared" name))]
      [(null? (layer_keys (top_layer state)))
       (lookup (next_in_state state) name
               (lambda (s v) (return (preserve_state state s) v)))]
      [(eq? name (car (layer_keys (top_layer state))))
       (if (null? (car (layer_values (top_layer state))))
         (error 'lookup (format "Variable ~a has not been assigned a value" name))
         (return state (car (layer_values (top_layer state)))))]
      [else (lookup (next_in_state state) name
                    (lambda (s v) (return (preserve_state state s) v)))])))

;update binding
; cps form of update
; Here value cannot be a statement, must be defined before calling update
(define update
  (lambda (state name value return)
    (cond
      [(null? state) (error 'update (format "Variable ~a has not been declared" name))]
      [(null? (layer_keys (top_layer state)))
       (update (next_in_state state) name value
               (lambda (s v) (return (preserve_state state s) v)))]
      [(eq? name (car (layer_keys (top_layer state))))
       (return (cons_to_state (next_in_state state)
                              (car (layer_keys (top_layer state)))
                              value)
               value)]
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
  (lambda (state statement)
    (let ([value (cadr statement)]
          [immediate_return (lambda (s v) v)])
      (interpret_statement state value immediate_return))))

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
    (let* ([iterate_while (lambda (s v) (interpret_while s statement return))]
           [continue (lambda (s v) (interpret_while s statement return))]
           [break (lambda (s v) (return s v))]
           [check_goto (lambda (s v)
                         (cond
                           [(not (box? v)) (iterate_while s v)]
                           [(eq? (unbox v) 'continue) (continue s '())]
                           [(eq? (unbox v) 'break) (break s '())]
                           [else (error 'interpret_while (format "Invalid goto: ~a" v))]))])
    (interpret_statement state (condition statement)
                         (lambda (s v)
                           (if (is_true? v)
                             (interpret_statement s (body statement) check_goto)
                             (return s '())))))))

; Begin block statement
(define begin_block
  (lambda (state statement return)
    (let ([block_state (cons '(() ()) state)]
          [block_body (cdr statement)]
          [pop_return (lambda (s v) (return (cdr s) v))])
      (interpret_statement block_state block_body pop_return))))

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

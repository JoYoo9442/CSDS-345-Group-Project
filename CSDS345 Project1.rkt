#lang racket
(require "simpleParser.rkt")
(require "lex.rkt")

; JUST for testing DELETE LATER
(define state '((RETURN) (())))

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
      ((null? (car statement_list)) state)   ;NOTE: is this the correct termination for empty statement-list? (I think so...)
      ((list? (car statement_list)) (parse_statement_list
                                     (cdr statement_list)
                                     (parse_statement (car statement_list) state))) 
      (else
       (error "parse_statement_list: Invalid parse - Expected a list, but got" (car statement_list))))))  ;error on invalid parse should never be thrown

;check which type of statement: parse statement
(define parse_statement
  (lambda (statement state)
    (cond
      ((eq? (car statement) 'var)    (declare statement state))
      ((eq? (car statement) '=)      (assign statement state))
      ((eq? (car statement) 'return) (return statement state))
      ((eq? (car statement) 'if)     (interpret_if statement state))
      ((eq? (car statement) 'while)  (interpret_while statement state))
      (else
       (error "parse_statement: Unknown statement" (statement))))))

;--------------statements-------------------------

;declaration statement
(define declare
  (lambda (statement state)
    (if (null? (cddr statement))
        (empty_declare statement state)
        (value_declare statement state))))

(define empty_declare
  (lambda (statement state)
    (bind (Mname statement) '() state)))

(define value_declare
  (lambda (statement state)
    (bind (Mname statement) (Mvalue statement) state)))

(define Mname cadr)
(define Mvalue caddr)


;assignment statement
(define assign
  (lambda (statement state)
    (if (eq? '() (lookup (Mname statement) state))
      (error 'declare "variable not declared")
      (update (Mname statement) (cadr statement) state))))
    

;return statement
(define return
  (lambda (statement state)
    (update
     'RETURN
     (M_integer statement state)
     state)))

;if statement
(define interpret_if
  (lambda (condition body else state)
    (if (M_boolean condition state)
      (parse_statement body state)
      (parse_statement else state))))
        

;while statement
(define interpret_while
  (lambda (condition body state)
    (if (interpret_if (M_boolean condition state))
      (parse_statement body state)  ; NOTE: not sure if this is right for when the condition for the while loop is true
      (state))))  ; NOTE: not sure if this is right for exitting the loop


;----------------bindings -------------


; function to just roll through the state: get the cdr of both the lists in state
(define next_in_state
  (lambda (state)
    (list (cdar state) (cdadr state))))

;create binding
; bind a name to a value and return the updated state
(define bind
  (lambda (name value state)
    (list (append (car state) (list name)) (append (cadr state) (list value)))))

;lookup binding
; return a value assigned to a name in the state
(define lookup
  (lambda (name state)
    (cond
      ((null? state) (error 'bad-op "Invalid operator")) ;error messages not updated
      ((eq? name (caar state)) (caadr state))
      (else (lookup name (next_in_state state))))))

;update binding
; cps form of update
(define update-cps
  (lambda (name value state return)
    (cond
      ((null? state) (error 'bad-op "Invalid operator")) ;fix error messsages pls
      ((null? (car state)) (return '() '()))
      ((eq? name (caar state)) (return (car state) (cons value (cdadr state))))
      (else (update-cps name value (next_in_state state) (lambda (v1 v2) (return (cons (caar state) v1) (cons (caadr state) v2))))))))

; make it so you just run update instead of update-cps
(define update
  (lambda (name value state)
    (update-cps name value state (lambda (v1 v2) (list v1 v2)))))


;---------------values-------------

(define operator car)
(define firstoperand cadr)
(define secondoperand caddr)

; NOTE: needs to be changed to take in a state for M_integer and M_boolean

;Minteger
(define M_integer
  (lambda (expression state)
    (cond
      ((number? expression) expression)
      ((eq? '+ (operator expression)) (+ (M_integer (firstoperand expression) state) (M_integer (secondoperand expression) state)))
      ((eq? '- (operator expression)) (- (M_integer (firstoperand expression) state) (M_integer (secondoperand expression) state)))
      ((eq? '* (operator expression)) (* (M_integer (firstoperand expression) state) (M_integer (secondoperand expression) state)))
      ((eq? '/ (operator expression)) (quotient (M_integer (firstoperand expression) state) (M_integer (secondoperand expression) state)))
      ((eq? '% (operator expression)) (remainder (M_integer (firstoperand expression) state) (M_integer (secondoperand expression) state)))
      
      (else (error 'bad-op "Invalid operator")))))

; make helper method that will be like evaluate operand


;Mboolean
(define M_boolean
  (lambda (expression state)
    (cond
      ((number? expression) expression)
      ((eq? '> (operator expression)) (> (M_boolean (firstoperand expression) state) (M_boolean (secondoperand expression) state)))
      ((eq? '< (operator expression)) (< (M_boolean (firstoperand expression) state) (M_boolean (secondoperand expression) state)))
      ((eq? '<= (operator expression)) (<= (M_boolean (firstoperand expression) state) (M_boolean (secondoperand expression) state)))
      ((eq? '>= (operator expression)) (>= (M_boolean (firstoperand expression) state) (M_boolean (secondoperand expression) state)))
      ((eq? '== (operator expression)) (eq? (M_boolean (firstoperand expression) state) (M_boolean (secondoperand expression) state)))
      ((eq? '!= (operator expression)) ((not eq? (M_boolean (firstoperand expression) state) (M_boolean (secondoperand expression) state))))
      (else (error 'bad-op "Invalid operator")))))

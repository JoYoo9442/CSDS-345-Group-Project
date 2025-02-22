#lang racket
(require "simpleParser.rkt")
(require "lex.rkt")

; JUST FOR TESTING DELETE LATER
(define state '((RETURN) ('())))

;the main interpret function
(define interpret
  (lambda (filename)            ;;TODO: define initial state in here
    (parse_statement_list
     (parser filename)
     (list '(RETURN) '(())))))

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
      ((eq? (car statement) 'if)     (if statement state))
      ((eq? (car statement) 'while)  (while statement state))
      (else
       (error "parse_statement: Unknown statement" (statement))))))

;--------------statements-------------------------

;declaration statement
(define declare
  (lambda (statement state)
    (if (null? (cdr statement))
      (bind (Mname statement) '() state)
      (bind (Mname statement) (cadr statement) state))))
(define Mname car)
(define Mint cadr)


;assignment statement
(define assign
  (lambda (statement state)
    (if (eq? '() (lookup (Mname statement) state))
      (error 'declare "variable not declared")
      (update (Mname statement) (cadr statement) state))))
    

;return statement
(define return
  (lambda (statement state)
    (lookup (Mname statement) state)))

;if statement
(define if
  (lambda (condition body else state)
    (if (M_boolean condition)
        

;while statement


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
  (lambda (expression)
    (cond
      ((number? expression) expression)
      ((eq? '+ (operator expression)) (+ (M_integer (firstoperand expression)) (M_integer (secondoperand expression))))
      ((eq? '- (operator expression)) (- (M_integer (firstoperand expression)) (M_integer (secondoperand expression))))
      ((eq? '* (operator expression)) (* (M_integer (firstoperand expression)) (M_integer (secondoperand expression))))
      ((eq? '/ (operator expression)) (quotient (M_integer (firstoperand expression)) (M_integer (secondoperand expression))))
      ((eq? '% (operator expression)) (remainder (M_integer (firstoperand expression)) (M_integer (secondoperand expression))))
      (else (error 'bad-op "Invalid operator")))))


;Mboolean
(define M_boolean
  (lambda (expression)
    (cond
      ((number? expression) expression)
      ((eq? '> (operator expression)) (> (M_boolean (firstoperand expression)) (M_boolean (secondoperand expression))))
      ((eq? '< (operator expression)) (< (M_boolean (firstoperand expression)) (M_boolean (secondoperand expression))))
      ((eq? '<= (operator expression)) (<= (M_boolean (firstoperand expression)) (M_boolean (secondoperand expression))))
      ((eq? '>= (operator expression)) (>= (M_boolean (firstoperand expression)) (M_boolean (secondoperand expression))))
      ((eq? '== (operator expression)) (eq? (M_boolean (firstoperand expression)) (M_boolean (secondoperand expression))))
      ((eq? '!= (operator expression)) ((not eq? (M_boolean (firstoperand expression)) (M_boolean (secondoperand expression)))))
      (else (error 'bad-op "Invalid operator")))))
#lang racket
(require "simpleParser.rkt")
(require "lex.rkt")

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
(define declaration
  (lambda (statement state)
    (if (null? (cdr statement))
      (bind (car statement) '() state)
      (bind (car statement) (cadr statement) state))))

;assignment statement

;return statement

;if statement

;while statement


;----------------bindings -------------

;create binding

;lookup binding

;update binding


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
      (else (error 'bad-op "Invalid operator")))))





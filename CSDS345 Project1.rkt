#lang racket
(require "simpleParser.rkt")
(require "lex.rkt")

(define interpret
  (lambda (filename)
    (parser filename)))


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





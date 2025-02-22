#lang racket
(require "simpleParser.rkt")

(define state '((RETURN) ('())))

(define next_in_state
  (lambda (state)
    (list (cdar state) (cdadr state))))

(define bind
  (lambda (name value state)
    (list (append (car state) (list name)) (append (cadr state) (list value)))))

(define lookup
  (lambda (name state)
    (cond
      ((null? state) (error 'bad-op "Invalid operator"))
      ((eq? name (caar state)) (caadr state))
      (else (lookup name (next_in_state state))))))

(define update-cps
  (lambda (name value state return)
    (cond
      ((null? state) (error 'bad-op "Invalid operator"))
      ((null? (car state)) (return '() '()))
      ((eq? name (caar state)) (return (car state) (cons value (cdadr state))))
      (else (update-cps name value (next_in_state state) (lambda (v1 v2) (return (cons (caar state) v1) (cons (caadr state) v2))))))))

(define update
  (lambda (name value state)
    (update-cps name value state (lambda (v1 v2) (list v1 v2)))))
    
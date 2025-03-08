#lang racket
(provide (all-defined-out))

(define ubd_error "ERROR (Usage before declaring)")
(define uba_error "ERROR (Usage before assignment)")
(define ubr_error "ERROR (redeclaration)")
(define normal_tests 20)
(define result_list
  (list
    150 -4 10 16 220 5 6 10 5 -39 ubd_error
    ubd_error uba_error ubr_error 'true 100
    'false 'true 128 12 30 11 1106 12 16 72
    21 164))

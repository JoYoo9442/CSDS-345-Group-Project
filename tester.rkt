#lang racket
(require "Project1.rkt")
(require "simpleParser.rkt")

(define result_list '(150 -4 10 16 220 5 6 10 5 -39 ERROR ERROR ERROR ERROR true 100 false true 128 12 30 11 1106 12 16 72 21 164))

(display "Enter test number (1-28): ")
(define testnum (read))
(define testnumstring "")
(if (< testnum 10) (set! testnumstring (string-append "0" (number->string testnum))) (set! testnumstring (number->string testnum)))

(define testfile (string-append "tests/test" testnumstring ".txt"))

(displayln "Parser Result: ")
(parser testfile)
(displayln "Interpreter Result: ")
(interpret testfile)
(displayln "Expected Result: ")
(displayln (list-ref result_list (- testnum 1)))

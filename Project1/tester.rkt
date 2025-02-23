#lang racket
(require "Project1.rkt")
(require "simpleParser.rkt")

(define result_list '(150 -4 10 16 220 5 6 10 5 -39 ERROR ERROR ERROR ERROR true 100 false true 128 12 30 11 1106 12 16 72 21 164))

(displayln "Enter test number (1-28) OR")
(displayln "Enter -1 for custom test (test.txt)")
(define testnum (read))
(define testnumstring "")
(cond
  ((< testnum 0) (set! testnumstring ""))
  ((< testnum 10) (set! testnumstring (string-append "0" (number->string testnum))))
  (else (set! testnumstring (number->string testnum))))

(define testfile (string-append "tests/test" testnumstring ".txt"))

(displayln "Parser Result: ")
(parser testfile)
(displayln "Interpreter Result: ")
(interpret testfile)

(if (> testnum 0)
  (begin
    (displayln "Expected Result: ")
    (displayln (list-ref result_list (- testnum 1))))
    (displayln "No expected result for custom test"))

#lang racket
(require "Project2.rkt")
(require "simpleParser.rkt")

(define result_list
  (list 20 164 32 2 'ERROR 25 21 6 -1 789 'ERROR 'ERROR 'ERROR 12 125 110 2000400 101 'ERROR 21))

(define create-file-str
  (lambda (testnum)
    (if (< testnum 10)
          (string-append "tests/test0" (number->string testnum) ".txt")
          (string-append "tests/test" (number->string testnum) ".txt"))))

(define test-project
  (lambda (testnum)
    (if (zero? testnum)
      (begin
        (display "Enter test number: ")
        (test-project (read)))
      (let ([filename (create-file-str testnum)])
        (displayln "Parser Result: ")
        (displayln (parser filename))
        (displayln "Expected Result: ")
        (displayln (list-ref result_list (- testnum 1)))
        (displayln "Interpreter Result: ")
        (displayln (interpret filename))))))

(test-project 0)

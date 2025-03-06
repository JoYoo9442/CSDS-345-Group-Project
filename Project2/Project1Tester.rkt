#lang racket
(require "Project2.rkt")
(require "simpleParser.rkt")

(define ubd_error "ERROR (Usage before declaring)")
(define uba_error "ERROR (Usage before assignment)")
(define ubr_error "ERROR (redeclaration)")
(define result_list (list 150 -4 10 16 220 5 6 10 5 -39 ubd_error ubd_error uba_error ubr_error 'true 100 'false 'true 128 12 30 11 1106 12 16 72 21 164))

(define dotest #t)
(define testnumstring "")
(define testnum 0)
(define testfile "")

(define create-file-str
  (lambda (testnum)
    (if (< testnum 10)
          (string-append "oldtests/test0" (number->string testnum) ".txt")
          (string-append "oldtests/test" (number->string testnum) ".txt"))))

(define test-project
  (lambda (testnum)
    (if (number? testnum)
      (if (zero? testnum)
        (begin
          (display "Enter test number: ")
          (test-project (read)))
        (let ([filename (create-file-str testnum)])
          (displayln "-----------------------------------")
          (displayln (format "Test ~a" testnum))
          (displayln "Parser Result: ")
          (displayln (parser filename))
          (displayln "Expected Result: ")
          (displayln (list-ref result_list (- testnum 1)))
          (displayln "Interpreter Result: ")
          (displayln (interpret filename))
          (displayln "-----------------------------------")
          (displayln "Enter test number to run again")
          (displayln "Regular: (1-20), Extra: (21-28)")
          (display "Or enter s to stop: ")
          (test-project (read))))
      (displayln "Invalid test number. Goodbye!"))))

(test-project 0)

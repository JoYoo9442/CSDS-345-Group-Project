#lang racket
(require "Project2.rkt")
(require "simpleParser.rkt")

(define testfolder "tests/")

(define path-list
  (lambda (folder)
    (map path->string
         (directory-list folder))))

(define display-folders-0
  (lambda (folder-list num)
    (if (null? folder-list)
      (displayln "---------------------")
      (begin
        (displayln (format "  ~a. ~a" num (car folder-list)))
        (display-folders-0 (cdr folder-list) (+ num 1))))))

(define display-folders
  (lambda (folder-list)
    (display-folders-0 folder-list 1)))

(define select-folder
  (lambda (folder-list)
    (displayln "Select a folder")
    (displayln "---------------------")
    (display-folders folder-list)
    (display "Enter folder number: ")
    (get-folder folder-list (- (read) 1))))

(define get-folder
  (lambda (folder-list num)
    (cond
      [(zero? num) (car folder-list)]
      [(null? folder-list) (displayln "Invalid folder number. Goodbye!")]
      [else (get-folder (cdr folder-list) (- num 1))])))

(define get-folder-path
  (lambda (folder)
    (string-append
      testfolder
      (select-folder (path-list folder)))))

(define testfolderpath (string-append (get-folder-path testfolder) "/"))

(define get-data
  (lambda (name)
    (dynamic-require (string-append testfolderpath "testData.rkt") name)))

(define result_list (get-data 'result_list))
(define normal_tests (get-data 'normal_tests))
(define numtests (length result_list))

(define create-file-str
  (lambda (testnum)
    (cond
      [(zero? testnum) (string-append testfolderpath "custom.txt")]
      [(< testnum 10) (string-append testfolderpath "test0" (number->string testnum) ".txt")]
      [else (string-append testfolderpath "test" (number->string testnum) ".txt")])))

(define valid_testnum?
  (lambda (testnum)
    (and (number? testnum)
         (<= 1 testnum numtests))))

(define do-test
  (lambda (testnum)
    (let ([filename (create-file-str testnum)])
      (displayln "-----------------------------------")
      (if (zero? testnum)
        (displayln "Custom Test:")
        (displayln (format "Test ~a:" testnum)))
      (displayln "------------------------")
      (displayln (file->string filename))
      (displayln "------------------------")
      (displayln "Parser Result: ")
      (displayln (parser filename))
      (if (zero? testnum)
        (void)
        (begin
          (displayln "Expected Result: ")
          (displayln (list-ref result_list (- testnum 1)))))
      (displayln "Interpreter Result: ")
      (displayln (interpret filename))
      (displayln "-----------------------------------"))))

(define test-project
  (lambda (testnum)
    (cond
      [(eq? 'program-start testnum)
         (displayln "Enter test number")
         (displayln (format "Custom: 0, Regular: 1-~a, Extra: ~a-~a" normal_tests (+ normal_tests 1) numtests))
         (test-project (read))]
      [(valid_testnum? testnum)
         (do-test testnum)
         (displayln "Enter test number to run again")
         (displayln (format "Custom: 0, Regular: 1-~a, Extra: ~a-~a" normal_tests (+ normal_tests 1) numtests))
         (display "Or enter s to stop: ")
         (test-project (read))]
      [else (displayln "Invalid test number. Goodbye!")])))

(test-project 'program-start)

#lang racket
(require "simpleParser.rkt")
(require "lex.rkt")

(define interpret
  (lambda (text)
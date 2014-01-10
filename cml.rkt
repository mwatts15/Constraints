#lang racket

(require "constraint-lang.rkt"
         "math-types.rkt"
         "constraint-types.rkt")

(provide evalFile)

(define types
  (hash '* Product
        '+ Sum
        '/ Quotient
        '- Difference
        'even Even
        '= Equal))

(define (read/f->c in)
  (define (loop)
    (let ([v (read in)])
      (if (eof-object? v)
        '()
        (cons v (loop)))))
  (loop))

(define (evalFile fname)
  (f->c (read/f->c (open-input-file fname)) types #:name 'Top))

(define inputFile
  (command-line
   #:program "cml"
   #:args (filename) ; expect one command-line argument: <filename>
   ; return the argument as a filename to compile
   filename))
(evalFile inputFile)

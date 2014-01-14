#lang racket

(require "constraint-lang.rkt"
         "math-types.rkt"
         "inequality.rkt"
         "geometry.rkt"
         "console-rep.rkt"
         "connector.rkt"
         "constraint-types.rkt")

(provide evalFile)

(define types
  (hash '* Product
        '+ Sum
        '/ Quotient
        '- Difference
        'even Even
        'array Array
        '< LessThan
        '> GreaterThan
        'dist Distance
        'point Point
        'is Equal))

(define (read/f->c in)
  (define (loop)
    (let ([v (read in)])
      (if (eof-object? v)
        '()
        (cons v (loop)))))
  (loop))

(define (evalFile fname)
  (let* ([cc (f->c (read/f->c (open-input-file fname)) types #:name 'Top)])
         (new cc)))

(define inputFile
  (command-line
   #:program "cml"
   #:args (filename) ; expect one command-line argument: <filename>
   ; return the argument as a filename to compile
   filename))
(evalFile inputFile)

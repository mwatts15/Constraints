#lang racket

(require "constraint-lang.rkt"
         "math-types.rkt"
         "inequality.rkt"
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
        'is Equal))

(define (read/f->c in)
  (define (loop)
    (let ([v (read in)])
      (if (eof-object? v)
        '()
        (cons v (loop)))))
  (loop))

(define (evalFile fname)
  (let* ([cc (f->c (read/f->c (open-input-file fname)) types #:name 'Top)]
         [c (new cc)])
    (for ([p (send c connectorNames)])
      (let ([connector (new Connector [name p])])
        (connect connector c p)
        (new ConsoleRep [c connector])))))

(define inputFile
  (command-line
   #:program "cml"
   #:args (filename) ; expect one command-line argument: <filename>
   ; return the argument as a filename to compile
   filename))
(evalFile inputFile)

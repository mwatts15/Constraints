#lang racket
(require racklog)
(require racket/math)
 
(define %parent
  (%rel ()
        [('bob 'mary)]
        [('alice 'mary)]))
(define %male 
  (%rel ()
        [('bob)]))
(define %female 
  (%rel ()
        [('alice)]))

(define %father
  (%rel (person child)
        [(person child)
         (%parent person child)
         (%male person)]))

(define %mother
  (%rel (person child)
        [(person child)
         (%parent person child)
         (%female person)]))
(define valBox
  (lambda (b)
    (printf "~a is a box~n" b)
    2))
(define box%
  (class object%
    (super-new)
    [field x y w h]))
(define hasValue
  (lambda (b v)
    (printf "~a has a value of ~a~n" b v)))
(define %boxing
  (%rel (j x)
    [(valBox j) (hasValue j x) %true]))
(define succ
  (lambda (x)
    (vector 'succ x)))
(define %add
  (%rel (x y z)
    [(0 y y)]
    [((succ x) y (succ z))
     (%add x y z)]))
(define kax
  (%which (x)
          (%add 0 (succ 0) x)))
(display kax)

#lang racket

(define A
  (class object%
    (super-new)
    (define x 0)
    (define (add-x v)
      (set! x (+ x v)))
    (define (show-x)
      (display x)(newline))
    (define (get-x)
      x)
    (public add-x get-x show-x)))
(define B
  (class A
    (super-new)
    (let ([x (send this get-x)])
      (eq? x 0))))
(define C
  (class B
    (super-new)
    (send this add-x 2)))

(eq? (send (new C) get-x) 2)


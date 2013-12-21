#lang racket

(require (prefix-in V: "value.rkt"))
(require (prefix-in C: "constraint.rkt"))
(require (prefix-in N: "control.rkt"))
(require (prefix-in R: "representation.rkt"))
(require racket/gui/base)
(provide (all-defined-out))

(define Rectangle
  (class object%
    (super-new)
    (init drawingContext)
    (define _dc drawingContext)
    (define _c (new C:Connector))
    (define _con (new N:RectangleControl [connector _c]))
    (define _rep (new R:Rectangle
                      [connector _c]
                      [drawingContext drawingContext]))
    (define (-> c . args)
      (apply dynamic-send _con c args))
    (define (get-dc)
      _dc)

    (let ([r (new V:Rectangle)])
      (set-field! x r 50)
      (set-field! y r 50)
      (set-field! w r 20)
      (set-field! h r 80)
      (send _c setValue! r _con))
    (public get-dc ->)))

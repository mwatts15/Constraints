#lang racket

(require (prefix-in V: "value.rkt"))
(require (prefix-in C: "constraint.rkt"))
(require (prefix-in N: "control.rkt"))
(require (prefix-in R: "representation.rkt"))
(require "connector.rkt")
(require racket/gui/base)
(provide (all-defined-out))

(define Object
  (class object% (super-new)
    (init drawingContext)
    (init representation
          control)
    (define _c (new Connector))
    (define _dc drawingContext)
    (define _con (new control [connector _c]))

    (define (-> c . args)
      (cond [(eq? c 'inside?)
             (let ([p (V:sub/pp (first args) (send _con getPos))])
               (dynamic-send _rep c p))]
            [else (apply dynamic-send _con c args)]))
    (define _rep (new representation 
                      [connector _c]
                      [drawingContext drawingContext]))
    (define (get-dc)
      _dc)
    (define (setup connector control representation)
      #t)
    (setup _c _con _dc)
    (public setup)
    (public get-dc ->)))

(define Rectangle
  (class Object
    (super-new [representation R:Rectangle]
               [control N:RectangleControl])
    (define/override (setup c ctrl rep)
      (let ([r (new V:Rectangle)])
        (set-field! x r 50)
        (set-field! y r 50)
        (set-field! w r 20)
        (set-field! h r 80)
        (send c setValue! r ctrl)))))
    

(define List
  (class Object
    (super-new [representation R:RectangleList]
               [control N:List])
    (define _car (new Connector))
    (define _cdr (new Connector))
    (define _lconstraint (new C:List))
    (define/override (setup c ctrl rep)
      (let ([l (list)]
            [head (new Connector)]
            [tail (new Connector)]
            [listConstraint (new C:List)])
        (connect c listConstraint 'list)
        (connect head listConstraint 'head)
        (connect tail listConstraint 'tail)
        (send c setValue! l ctrl)))))

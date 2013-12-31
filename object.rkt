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
          control
          initialValue)
    (init [name "Object"])
    (define _name name)
    (define _c (new Connector))
    (define _dc drawingContext)
    (define _con (new control [connector _c]))
    (define _rep (new representation 
                      [connector _c]
                      [drawingContext drawingContext]))
    (define _exposedConnectors (make-hash))
    (define (-> c . args)
      (cond [(eq? c 'inside?)
             (let ([p (V:sub/pp (first args) (send _con getPos))])
               (dynamic-send _rep c p))]
            [else (apply dynamic-send _con c args)]))
    (define (getConnectors)
      (dict-keys _exposedConnectors))
    (define (getConnector cname)
      (dict-ref _exposedConnectors cname))
    (define (exposeConnector c name)
      (dict-set! _exposedConnectors name c))
    (define (get-dc)
      _dc)
    (define (setup connector control representation)
      #t)
    (setup _c _con _dc)
    (send _c setValue! initialValue)
    (public setup)
    (public exposeConnector getConnector getConnectors)
    (public get-dc ->)))

(define Rectangle
  (class Object
    (super-new [representation R:Rectangle]
               [control N:RectangleControl]
               [initialValue V:Rectangle])))
    
(define List
  (class Object
    (super-new [representation R:RectangleList]
               [control N:List]
               [initialValue '()])
    (inherit exposeConnector)
    (define/override (setup c ctrl rep)
      (let ([head (new Connector)]
            [tail (new Connector)]
            [listConstraint (new C:List)])
        (connect c listConstraint 'list)
        (connect head listConstraint 'head)
        (connect tail listConstraint 'tail)
        (exposeConnector head 'head)
        (exposeConnector tail 'tail)))))

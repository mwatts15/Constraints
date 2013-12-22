#lang racket
(require "value.rkt")
(require "connector.rkt")
(provide (all-defined-out))
; queries and actions on rectangles
(define List
  (class object%
    (super-new)
    [init connector]
    (define _c connector)
    (define _p (make-point 0 0))
    (define (getPos)
      _p)
    (define (setPos p)
      (set! _p p))

    (define (_cons o)
      (let ([l (send _c getValue)])
        (send _c forgetValue! this)
        (send _c setValue! (cons o l) this)))
    (public getPos setPos (_cons cons))))

(define RectangleControl
  (class object%
    (super-new)
    [init connector]
    (define _c connector)

    (define (setWidth w)
      (let ([r (send _c getValue)])
        (send _c forgetValue! this)
        (set-field! w r w) 
        (send _c setValue! r this)))

    (define (setHeight h)
      (let ([r (send _c getValue)])
        (send _c forgetValue! this)
        (set-field! h r h) 
        (send _c setValue! r this)))

    (define (setPos p)
      (when (send _c hasValue?)
        (let ([r (send _c getValue)]
              [px (send p get-x)]
              [py (send p get-y)])
          (send _c forgetValue! this)
          (set-field! x r px) 
          (set-field! y r py)
          (send _c setValue! r this))))

    (define (getPos)
      (when (send _c hasValue?)
        (let* ([r (send _c getValue)]
               [x (get-field x r)]
               [y (get-field y r)])
          (make-point x y))))

    (public setHeight setWidth setPos getPos)))

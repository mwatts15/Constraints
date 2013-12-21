#lang racket
(require "value.rkt")
(require "constraint.rkt")
(provide (all-defined-out))
; queries and actions on rectangles
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

    (define (inside? p)
      (if (send _c hasValue?)
        (let* ([r (send _c getValue)]
               [px (send p get-x)]
               [py (send p get-y)]
               [x (get-field x r)]
               [y (get-field y r)]
               [w (get-field w r)]
               [h (get-field h r)])
          (and (> px x) (> py y) (< px (+ x w)) (< py (+ y h))))
        #f))
    (public setHeight setWidth setPos getPos inside?)))

#lang racket

(require "constraint-base.rkt"
         "constraint-lang.rkt"
         "world.rkt"
         (prefix-in V: "value.rkt")
         "math-types.rkt")

(provide Point
         LeftOf
         Square
         Distance)


(define Point
  (class Constraint
    (super-new [ports '(pt x y)] [name 'Point])
    (inherit getPort)
    (define/augment (resolve)
      (let* ([p (getPort 'pt)]
             [y (getPort 'y)]
             [x (getPort 'x)]
             [pv (send p getValue)]
             [yv (send y getValue)]
             [xv (send x getValue)])
        (cond [(is-set? pv)
               (send x setValue! (get-field x pv) this)
               (send y setValue! (get-field y pv) this)]
              [(and (is-set? xv)
                    (is-set? yv))
               (let ([newPoint (V:make-point xv yv)])
                 (send p setValue! newPoint this))])))))

(define Square
  (class Constraint
    (super-new [ports '(ob side)] [name 'Square])
    (inherit getPort)
    (define/augment (resolve)
      (let* ([s (getPort 'side)]
             [r (getPort 'ob)]
             [sv (send s getValue)]
             [rv (send r getValue)])
        (cond [(is-set? sv)
               (let ([rect (new V:Rectangle)])
                 (set-field! w rect sv)
                 (set-field! h rect sv)
                 (send r setValue! rect this))]
              [(is-set? rv)
               (let* ([rvWidth (get-field w rv)]
                      [rect (new V:Rectangle)])
                 (set-field! h rect rvWidth)
                 (set-field! w rect rvWidth)
                 (send s setValue! rvWidth this)
                 (send r setValue! rect this))])))))
(define Distance
  (f->c 
    '((point (x xp) (y yp) (pt point1))
      (point (x xq) (y yq) (pt point2))
      (- (lhs xp) (rhs xq) (res dx))
      (- (lhs yp) (rhs yq) (res dy))
      (* (lhs dx) (rhs dx) (res dx^2))
      (* (lhs dy) (rhs dy) (res dy^2))
      (+ (lhs dx^2) (rhs dy^2) (res distance)))
    `((point . ,Point)
      (- . ,Difference)
      (+ . ,Sum)
      (* . ,Product))
    #:name 'Distance))

(define LeftOf
  (f->c
    '((square (ob s) (side l))
      (square (ob r) (side k))
      (at (loc p) (ob s) (world w))
      (point (pt p) (x x) (y y))
      (+ (lhs l) (rhs x) (res qx))
      (point (pt q) (x qx) (y y))
      (at (loc q) (ob r) (world w)))
    `((square . ,Square)
      (at . ,At)
      (+ . ,Sum)
      (point . ,Point))
    #:name 'LeftOf))

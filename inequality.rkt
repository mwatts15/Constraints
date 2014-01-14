#lang racket

(require "constraint-base.rkt"
         "constraint-lang.rkt"
         "value.rkt")
(provide LessThan GreaterThan)
(define LessThan
  (class Constraint
    (super-new [ports '(lesser greater)] [name 'LessThan])
    (inherit getPort)
    (define/augment (resolve)
      (let* ([l (getPort 'lesser)]
             [g (getPort 'greater)]
             [lv (send l getValue)]
             [gv (send g getValue)])
        (cond [(and (is-set? lv) (number? lv))
               (let* ([s (new Range [start lv] [end +inf.0])]
                      [sp (if (is-set? gv) (send gv intersect s) s)])
                 (send g setValue! sp this))]
              [(and (is-set? gv) (number? gv))
               (let* ([s (new Range [start -inf.0] [end gv])]
                      [sp (if (is-set? lv) (send lv intersect s) s)])
                 (send l setValue! sp this))])))))
(define GreaterThan
  (f->c '((< (lesser greater) (greater lesser)))
        `((< . ,LessThan))
        #:name 'GreaterThan))

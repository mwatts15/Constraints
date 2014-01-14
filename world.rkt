#lang racket

(require "constraint-base.rkt"
         (prefix-in V: "value.rkt"))
(provide At)

(define At
  (class Constraint
    (super-new [ports '(loc ob world)] [name 'At])
    (inherit getPort)
    (define/augment (resolve)
      (let* ([l (getPort 'loc)]
             [o (getPort 'ob)]
             [w (getPort 'world)]
             [lv (send l getValue)]
             [ov (send o getValue)]
             [wv (send w getValue)])
        (cond [(and (is-set? lv)
                    (is-set? wv)
                    (equal? 1 (length (send wv getObjectsAt lv))))
               (send o setValue! (first (send wv getObjectsAt lv)) this)]
              [(and (is-set? lv)
                    (is-set? ov))
               (let ([newWorld (if (unset? wv)
                                 (new V:World)
                                 (new V:World [oldWorld wv]))])
                 (send newWorld placeObject ov lv)
                 (send w setValue! newWorld this))]
              [(and (is-set? wv)
                    (is-set? ov)
                    (is-set? (send wv getLocationOf ov)))
               (send l setValue! (send wv getLocationOf ov) this)])))))

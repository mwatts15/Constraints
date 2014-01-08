#lang racket

(require (only-in "constraint-base.rkt" Constraint)
         "unset.rkt"
         (prefix-in V: "value.rkt"))
(provide List
         Array
         Square
         At
         Point
         Square)

(define-syntax-rule (PredicateConstraint name pred? satisfier)
  (define name
    (class Constraint
      (super-new (ports '(value result)))
      (inherit getPort)

      (define/override (resolve)
        (let* ([r (getPort 'result)]
               [p (getPort 'value)]
               [rval (send r getValue)]
               [pval (send p getValue)])
          (if (send p hasValue?)
            (if (pred? pval)
              (send r setValue! true this)
              (send r setValue! false this))
            (send p setValue! (satisfier pval) this)))))))

(PredicateConstraint Odd odd? thunk)

(define List
  (class Constraint
    (super-new (ports '(head tail list)) [name 'List])
    (inherit getPort)
    (define/override (resolve)
      (let ([o (getPort 'list)]
            [t (getPort 'tail)]
            [f (getPort 'head)])
        (let ([ov (send o getValue)]
              [tv (send t getValue)]
              [fv (send f getValue)])
          (cond [(and (is-set? tv)
                      (is-set? fv))
                 (send o setValue! (cons fv tv) this)]
                [(is-set? ov)
                 (when (not (empty? ov))
                   (send t setValue! (rest ov) this)
                   (send f setValue! (first ov) this))]))))))
(define Array
  (class Constraint
    (super-new [ports '(index value array)] [name 'Array])
    (inherit getPort)
    (define/override (resolve)
      (let ([a (getPort 'array)]
            [i (getPort 'index)]
            [v (getPort 'value)])
        (let ([av (send a getValue)]
              [iv (send i getValue)]
              [vv (send v getValue)])
          (cond [(and (is-set? vv)
                      (is-set? iv)
                      (is-set? av))
                 (let ([vec (send a getValue)])
                   (vector-set! vec iv vv) 
                   (send a setValue! vec this))]
                [(and (is-set? av)
                      (is-set? iv))
                 (let ([vec (send a getValue)])
                   (send v setValue! (vector-ref vec iv) this))]
                [(and (is-set? av)
                      (is-set? vv))
                 (let ([vec (send a getValue)])
                   (send i setValue! (vector-member vv vec) this))]))))))
(define Square
  (class Constraint
    (super-new [ports '(ob side)] [name 'Square])
    (inherit getPort)
    (define/override (resolve)
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
(define At
  (class Constraint
    (super-new [ports '(loc ob world)] [name 'At])
    (inherit getPort)
    (define/override (resolve)
      (let* ([l (getPort 'loc)]
             [o (getPort 'ob)]
             [w (getPort 'world)]
             [lv (send l getValue)]
             [ov (send o getValue)]
             [wv (send w getValue)])
        (cond [(and (is-set? lv)
                    (is-set? wv)
                    (is-set? (send wv getObjectsAt lv)))
               (send o setValue! (send wv getObjectsAt lv) this)]
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
(define Point
  (class Constraint
    (super-new [ports '(pt x y)] [name 'Point])
    (inherit getPort)
    (define/override (resolve)
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

; holds a constant and matches the value of its sole connector.
; change the value though the methods setValue! and forgetValue!
(define Constant
  (class Constraint
    (super-new (ports '(out)) [name 'Constant])
    (inherit getPort)
    (define v unset)

    (define (set newval)
      (send (getPort 'out) forgetValue! this)
      (and (not (eq? newval unset))
           (begin (set! v newval)
                  (send (getPort 'out) setValue! v this))))

    (define (getValue)
      v)

    (define/override (resolve)
      (let* ([p (getPort 'out)])
        (and (not (unset? v))
             (send p setValue! v this))))

    (public
      (set setValue!)
      getValue)))

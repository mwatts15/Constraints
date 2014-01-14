#lang racket

(require (only-in "constraint-base.rkt" Constraint)
         "unset.rkt"
         "constraint-lang.rkt"
         "math-types.rkt"
         (prefix-in V: "value.rkt"))

(provide List
         Array)

(define-syntax-rule (PredicateConstraint name pred? satisfier)
  (define name
    (class Constraint
      (super-new (ports '(value result)))
      (inherit getPort)

      (define/augment (resolve)
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
    (define/augment (resolve)
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
    (define/augment (resolve)
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

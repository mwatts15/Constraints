#lang racket
(require "constraint.rkt"
         "unset.rkt")
(provide (all-defined-out))
; return boolean values and used in boolean algebras

(define Not
  (class Constraint
    (super-new (ports '(arg out))(name 'Not))
    (inherit getPort)
    (define/augment (resolve)
      (let ([arg (getPort 'arg)]
            [out (getPort 'out)])
        (if (or (send arg hasValue?) (send out hasValue?))
          (cond [(send arg hasValue?)
                 (send out setValue! (not (send arg getValue)) this)]
                [(send out hasValue?)
                 (send arg setValue! (not (send out getValue)) this)])
          false)))))

(define Or
  (class Constraint
    (super-new (ports '(lhs rhs out))(name 'Or))
    (inherit getPort)

    ; resolves the value at the given port if possible
    (define/augment (resolve)
      (let* ([a1 (getPort 'lhs)]
             [a2 (getPort 'rhs)]
             [o (getPort 'out)]
             [v1 (send a1 getValue)]
             [v2 (send a2 getValue)]
             [vo (send o getValue)])
        (cond [(and (send a1 hasValue?) (send a2 hasValue?))
               (send o setValue! (or v1 v2) this)]
              [(or (and (send a1 hasValue?) (eq? true v1))
                   (and (send a2 hasValue?) (eq? true v2)))
               (send o setValue! true this)]
              [(and (send o hasValue?) (eq? false vo))
               (send a1 setValue! false this)
               (send a2 setValue! false this)]
              [(and (send a1 hasValue?) (send o hasValue?) (not v1))
               (send a2 setValue! vo this)]
              [(and (send a2 hasValue?) (send o hasValue?) (not v2))
               (send a1 setValue! vo this)])))))

(define And
  (class Constraint
    (super-new (ports '(lhs rhs out))(name 'And))
    (inherit getPort)

    ; resolves the value at the given port if possible
    (define/augment (resolve)
      ;(display "calling resolve for And")(newline)
      (let* ([a1 (getPort 'lhs)]
             [a2 (getPort 'rhs)]
             [o (getPort 'out)]
             [v1 (send a1 getValue)]
             [v2 (send a2 getValue)]
             [vo (send o getValue)])
        (cond [(and (is-set? v1)
                    (is-set? v2))
               (send o setValue! (and v1 v2) this)]
              [(or (eq? false v1)
                   (eq? false v2))
               (send o setValue! false this)]
              [(eq? true vo)
               (send a1 setValue! true this)
               (send a2 setValue! true this)]
              [(and (is-set? vo)
                    (eq? true v1))
               (send a2 setValue! vo this)]
              [(and (is-set? vo)
                    (eq? true v2))
               (send a1 setValue! vo this)])))))

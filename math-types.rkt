#lang racket

(require "constraint-lang.rkt"
         "constraint-base.rkt"
         "connector.rkt"
         "unset.rkt"
         racket/match)

(provide Difference
         Product
         Quotient
         Sum
         Negation
         Even
         Equal)

(define Product
  (class Constraint
    (super-new (ports '(lhs rhs res))(name 'Product))
    (inherit getPort)
    (define/augment (resolve)
      (let* ([l (getPort 'lhs)]
             [r (getPort 'rhs)]
             [p (getPort 'res)]
             [lv (send l getValue)]
             [rv (send r getValue)]
             [pv (send p getValue)])
        (cond [(or (and (is-set? lv) (zero? lv))
                   (and (is-set? rv) (zero? rv)))
               (send p setValue! 0 this)]
              [(and (is-set? lv)
                    (is-set? rv))
               (send p setValue! (* lv rv) this)]
              [(and (is-set? lv)
                    (is-set? pv))
               (unless (eq? lv 0)
                 (send r setValue! (/ pv lv) this))]
              [(and (is-set? rv)
                    (is-set? pv))
               (unless (eq? rv 0)
                 (send l setValue! (/ pv rv) this))])))))
(define Sum
  (class Constraint
    (super-new (ports '(lhs rhs res))(name 'Sum))
    (inherit getPort)
    (define/augment (resolve)
      (let ([l (getPort 'lhs)]
            [r (getPort 'rhs)]
            [s (getPort 'res)])
        (let ([lv (send l getValue)]
              [rv (send r getValue)]
              [sv (send s getValue)])
          (cond [(and (is-set? lv)
                      (is-set? rv))
                 (send s setValue! (+ lv rv) this)]
                [(and (is-set? lv)
                      (is-set? sv))
                 (send r setValue! (- sv lv) this)]
                [(and (is-set? rv)
                      (is-set? sv))
                 (send l setValue! (- sv rv) this)]))))))
(define Negation
  (class Constraint
    (super-new (ports '(arg negation))(name 'Neg))
    (inherit getPort)
    (define/augment (resolve)
      (let ([i (getPort 'arg)]
            [o (getPort 'negation)])
        (let ([iv (send i getValue)]
              [ov (send o getValue)])
          (cond [(is-set? iv)
                 (send o setValue! (- iv) this)]
                [(is-set? ov)
                 (send i setValue! (- ov) this)]))))))

(define Even
  (class Constraint
    (super-new (ports '(arg)) (name 'Even))
    (inherit getPort)
    (define/augment (resolve)
      (let ([i (getPort 'arg)])
        (let ([iv (send i getValue)])
          (when (integer? iv)
            (unless (even? iv)
              (send i setValue! (* 2 iv) this))))))))


(define Equal
  (class Constraint
    (super-new (ports '(lhs rhs))(name 'Equal))
    (inherit getPort)
    (define/augment (resolve)
      (let ([lhs (getPort 'lhs)]
            [rhs (getPort 'rhs)])
        (let ([lv (send lhs getValue)]
              [rv (send rhs getValue)])
          (cond [(is-set? lv)
                 (send rhs setValue! lv this)]
                [(is-set? rv)
                 (send lhs setValue! rv this)]))))))

(define Difference
  (f->c 
    '((+ (res lhs) (lhs rhs) (rhs res)))
    `((+ . ,Sum))
    #:name 'Difference))

(define Quotient
  (f->c 
    '((* (res lhs) (lhs rhs) (rhs res)))
    `((* . ,Product))
    #:name 'Quotient))

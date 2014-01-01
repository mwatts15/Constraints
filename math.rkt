#lang racket

(require "constraint.rkt"
         "connector.rkt"
         "unset.rkt")
(require racket/match)

(provide (all-defined-out))
(define Product
  (class Constraint
    (super-new (ports '(lhs rhs res))(name 'Product))
    (inherit getPort)
    (define/override (resolve)
      (let* ([l (getPort 'lhs)]
             [r (getPort 'rhs)]
             [p (getPort 'res)]
             [lv (send l getValue)]
             [rv (send r getValue)]
             [pv (send p getValue)])
        (cond [(and (is-set? lv)
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
    (define/override (resolve)
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
(define Difference
  (class Constraint
    (super-new (ports '(lhs rhs res))(name 'Diff))
    (inherit getPort)
    (define/override (resolve)
      (let ([l (getPort 'lhs)]
            [r (getPort 'rhs)]
            [d (getPort 'res)])
        (let ([lv (send l getValue)]
              [rv (send r getValue)]
              [dv (send d getValue)])
          (cond [(and (is-set? lv)
                      (is-set? rv))
                 (send d setValue! (- lv rv) this)]
                [(and (is-set? lv)
                      (is-set? dv))
                 (send r setValue! (- (- dv lv)) this)]
                [(and (is-set? rv)
                      (is-set? dv))
                 (send l setValue! (+ dv rv) this)]))))))
(define Quotient
  (class Constraint
    (super-new (ports '(lhs rhs res))(name 'Quot))
    (inherit getPort)
    (define/override (resolve)
      (let ([l (getPort 'lhs)]
            [r (getPort 'rhs)]
            [q (getPort 'res)])
        (let ([lv (send l getValue)]
              [rv (send r getValue)]
              [qv (send q getValue)])
          (cond [(and (is-set? lv)
                      (is-set? rv))
                 (if (zero? rv)
                   (send q setValue! 'undef this)
                   (send q setValue! (/ lv rv) this))]
                [(and (is-set? lv)
                      (is-set? qv))
                 (if (zero? qv)
                   (send l setValue! 0 this)
                   (send r setValue! (/ lv qv) this))]
                [(and (is-set? rv)
                      (is-set? qv))
                 (send l setValue! (* qv rv) this)]))))))
(define Negation
  (class Constraint
    (super-new (ports '(arg negation))(name 'Neg))
    (inherit getPort)
    (define/override (resolve)
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
    (define/override (resolve)
      (let ([i (getPort 'arg)])
        (let ([iv (send i getValue)])
          (unless (even? iv)
            (send i setValue! (* 2 iv) this)))))))


(define (toInfix expr)
  (match expr
    [`(,op ,lhs ,rhs) `(,(toInfix lhs) ,op ,(toInfix rhs))]
    [x x]))
(define (mathOpt expr)
  (match (match expr
                [`(+ ,x ,x) `(* 2 ,x)]
                [`(+ ,x (+ ,x ,y)) `(+ (* 2 ,x) ,y)]
                [`(+ ,x (- ,x ,y)) `(- ,y)]
                [`(- ,x (+ ,x ,y)) y]
                [`(/ 1 (/ ,x ,y)) `(/ ,y ,x)]
                [`(/ ,x 1) x]
                [`(,(or '+ '-) ,x 0) x]
                [`(* ,x 1) x]
                [`(* ,x 0) 0]
                [`(- ,x ,x) 0]
                [`(+ (- ,x) ,y) `(- ,y ,x)]
                [`(- ,x (- ,y)) `(+ ,x ,y)]
                [`(= ,(and (not 0) x) ,y) `(= 0 (- ,y ,x))]
                ;combining terms
                [`(,(and (or '+ '-) op)
                    (* ,c1 ,x)
                    (* ,c2 ,x)) `(* (,op ,c1 ,c2) ,x)]
                [`(,(and (or '+ '-) op)
                    ,x
                    (* ,c2 ,x)) `(* (,op 1 ,c2) ,x)]
                [`(,(and (or '+ '-) op)
                    (* ,c1 ,x)
                    ,x) `(* (,op ,c1 1) ,x)]
                ; heuristic: move numbers to the outside
                [`(,(and (or '+ '-) op1)
                    (,(and (or '+ '-) op2) ,x ,(? number? y))
                    ,(? (negate number?) z))
                  `(,op2 (,op1 ,x ,z) ,y)]
                ; the 'infamous' flip rule. probably shouldn't be here...
                [`(,(and (or '+ '*) op) ,x ,y) `(,op ,y ,x)]
                [x x])
         [`(,op ,x ...) (cons op (map mathOpt x))]
         [x x]))

(define Equal
  (class Constraint
    (super-new (ports '(lhs rhs))(name 'Equal))
    (inherit getPort)
    (define/override (resolve)
      (let ([lhs (getPort 'lhs)]
            [rhs (getPort 'rhs)])
        (let ([lv (send lhs getValue)]
              [rv (send rhs getValue)])
          (cond [(is-set? lv)
                 (send rhs setValue! lv this)]
                [(is-set? rv)
                 (send lhs setValue! rv this)]))))))

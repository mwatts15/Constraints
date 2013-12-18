#lang racket

(require "constraint.rkt")
(require "variables.rkt")
(require (only-in (file "bool.rkt") Equal And))

(provide (all-defined-out))
(define Product
  (class Constraint
    (super-new (ports '(lhs rhs product)))
    (inherit getPort)
    (define/override (resolve)
      (let ([l (getPort 'lhs)]
            [r (getPort 'rhs)]
            [p (getPort 'product)])
        (let ([lv (send l getValue)]
              [rv (send r getValue)]
              [pv (send p getValue)])
          (cond [(and (is-set? lv)
                      (is-set? rv))
                 (send p setValue! (* lv rv) this)]
                [(and (is-set? lv)
                      (is-set? pv))
                 (send r setValue! (/ pv lv) this)]
                [(and (is-set? rv)
                      (is-set? pv))
                 (send l setValue! (/ pv rv) this)]))))))
(define Sum
  (class Constraint
    (super-new (ports '(lhs rhs sum)))
    (inherit getPort)
    (define/override (resolve)
      (let ([l (getPort 'lhs)]
            [r (getPort 'rhs)]
            [s (getPort 'sum)])
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
    (super-new (ports '(lhs rhs difference)))
    (inherit getPort)
    (define/override (resolve)
      (let ([l (getPort 'lhs)]
            [r (getPort 'rhs)]
            [d (getPort 'difference)])
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
    (super-new (ports '(lhs rhs quotient)))
    (inherit getPort)
    (define/override (resolve)
      (let ([l (getPort 'lhs)]
            [r (getPort 'rhs)]
            [q (getPort 'quotient)])
        (let ([lv (send l getValue)]
              [rv (send r getValue)]
              [qv (send q getValue)])
          (cond [(and (is-set? lv)
                      (is-set? rv))
                 (send q setValue! (/ lv rv) this)]
                [(and (is-set? lv)
                      (is-set? qv))
                 (send r setValue! (/ lv qv) this)]
                [(and (is-set? rv)
                      (is-set? qv))
                 (send l setValue! (* qv rv) this)]))))))
(define Negation
  (class Constraint
    (super-new (ports '(arg negation)))
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

(define (mathOpt expr)
  (match expr
    [`(+ ,x ,x) `(* 2 ,x)]
    [`(+ ,x (+ ,x ,y)) `(+ (* 2 ,x) ,y)]
    [`(+ ,x (- ,x ,y)) `(- ,y)]
    [`(- ,x (+ ,x ,y)) y]
    [`(/ 1 (/ ,x ,y)) `(/ ,y ,x)]
    [`(/ ,x 1) x]
    [`(- ,x 0) x]
    [`(+ ,x 0) x]
    [`(* ,x 1) x]
    [`(* ,x 0) 0]
    [`(- ,x ,x) 0]
    [`(+ (- ,x) ,y) `(- ,y ,x)]
    [`(- ,x (- ,y)) `(+ ,x ,y)]
    [(list x y ...) `(,x . ,(map mathOpt y))]
    [x x]))

(define (f->c formula [parentConstraint #f] [side #f])
  (define ops ; indexed by number of operands
    (vector #f
            (hash '- (list Negation 'negation))
            (hash '+ (list Sum 'sum)
                  '- (list Difference 'difference)
                  '/ (list Quotient 'quotient)
                  '= (list Equal 'out)
                  'and (list And 'out)
                  '* (list Product 'product))))
  (define (var? expr) 
    (and (symbol? expr) 
         (regexp-match #rx"[a-z]+" (symbol->string expr))))

  (define (vars expr)
    (cond [(list? expr) 
           (apply append (map vars expr))]
          [(var? expr) (list expr)]
          [else '()]))

  (define simplifiedFormula (mathOpt formula))

  (define external
    (class Constraint
      [init vstore]
      (define vs vstore)
      (super-new [ports (dict-keys vstore)])
      ; we don't have to do anything here because the connectors will activate 
      ; the correct internal constraints
      (define/override (resolve)
        #t)
      (define/override (attach p c)
        (for ([(ob innerName) (dict-ref vs p)])
          (connect c ob innerName)))))

  (define vstore (make-hash))
  (for ([v (vars formula)])
    (dict-set! vstore v (make-hash)))

  (define (sub f p s)
    (cond [(list? f)
           (let* ([c-type (first f)]
                  [d (dict-ref (dict-ref ops (sub1 (length f))) c-type)]
                  [c (new (first d))]
                  [output (second d)])
             (if p 
               (connectConstraints c output p s)
               (dict-set! vstore 'result (make-hash (list (cons c output)))))
             (cond [(eq? (length f) 3) ; binary operations
                    (sub (second f) c 'lhs)
                    (sub (third f) c 'rhs)]
                   [(eq? (length f) 2) ; unary operations
                    (sub (second f) c 'arg)]))]
          [(and (var? f) p)
           (let* ([internals (dict-ref vstore f)])
             (dict-set! internals p s))]
          [(and (number? f) p)
           ; make a constant and attach it
           (let ([c (new Constant)]
                 [con (new Connector)])
             (connect con c 'out)
             (send c setValue! f)
             (connect con p s))]))
  (sub simplifiedFormula parentConstraint side)
  (new external [vstore vstore]))

#lang racket

(require "math.rkt")
(require "variables.rkt")
(require "constraint.rkt")
(require "console-rep.rkt")


(define (equation eqn settings )
  (let ([c (f->c eqn)])
    (let ([vars (for/hash ([v (send c connectorNames)])
                  (let ([var (new Variable [name v])])
                    (new ConsoleRep [c var])
                    (connect var c v)
                    (values v var)))])
      (for ([(name value) settings])
        (send (dict-ref vars name) setValue! value 'user))
    c)))

(define GraphRep
  (class object%
    (super-new)
    (init-field c)
    (connect c this)
    (define (attach p con)
      (set! c con))
    (define (reevaluate) (resolve))
    (define (disconnect port)
      (error "nope"))
    (define (resolve)
      (if (and (send c hasValue?) (number? (send c getValue)))
        (display (make-list (send c getValue) (send c getName)))
        (display (list (send c getName))))
      (newline))
    (public resolve reevaluate attach disconnect)))

(equation '(- x (+ x (- x (* y z))))
          (hash 'x 7 
                'result 123
                'z 23))
(equation '(- x 4) (hash 'x 7))
; physics
;(let ([e (equation '(and (= x (+ (* v t) x0))
                         ;(= v (+ (* a t) v0)))
                   ;(hash 'x0 0 'v0 12 'a 4 'result #t))]
  ;(connect tvar e 't)
  ;(for ([t (in-range 10)])
    ;(display `(t is ,t))(newline)
    ;(send tvar setValue! t 'user)))
;(let ([e (equation '(= x (+ (* v t) x0))
                   ;(hash 'x0 0 'v 2 'result #t))]
(let ([e (new Equal)]
      [s (new Sum)]
      [p (new Product)]
      [x0 (new Variable [name 'x0])]
      [x (new Variable [name 'x])]
      [v (new Variable [name 'v])]
      [r (new Variable [name 'result])]
      [tvar (new Variable [name 't])])
  (connect x e 'lhs)
  (connectConstraints s 'sum e 'rhs)
  (connectConstraints s 'lhs p 'product)
  (connect v p 'lhs)
  (connect tvar p 'rhs)
  (connect x0 s 'rhs)
  (connect r e 'out)
  (send v setValue! 2 'user)
  (send r setValue! #t 'user)
  (send x0 setValue! 0 'user)
  (for ([t (in-range 10)])
    (display `(t is ,t))(newline)
    (send tvar setValue! t 'user)))

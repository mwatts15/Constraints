#lang racket

(require "math.rkt")
(require "variables.rkt")
(require "constraint.rkt")
(require "console-rep.rkt")


(define (equation eqn settings [repKind ConsoleRep])
  (define vars (make-hash))
  (let ([c (f->c eqn)])
    (for ([v (send c connectorNames)])
      (let ([var (new Variable [name v])])
        (new repKind [c var])
        (dict-set! vars v var)
        (connect var c v)))
    (for ([(name value) settings])
      (send (dict-ref vars name) setValue! value 'user))
    (newline)))
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
(mathOpt '(= y (+ x x)))
; physics
(for ([t (in-range 10)])
  (equation '(and (= x (+ (* v t) x0))
                  (= v (+ (* a t) v0)))
            (hash 'x0 0 'v0 12 't t 'a 2 'result #t)))


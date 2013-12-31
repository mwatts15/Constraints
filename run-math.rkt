#lang racket

(require "math.rkt")
(require "variables.rkt")
(require "constraint.rkt")
(require "console-rep.rkt")

(define (equation eqns settings)
  (let ([c (apply f->c eqns)])
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
                'z 23))
(equation '(- x 4) (hash 'x 7))

; physics
(let ([e (equation '((= x (+ (* v t) x0))
                     (= v (+ (* a t) v0)))
                   (hash 'x0 0 'v0 12 'a 4))]
      [tvar (new Variable [name 't])])
      (connect tvar e 't)
      (for ([t (in-range 10)])
        (display `(t is ,t))(newline)
        (with-output-to-file "graph.dot" #:exists 'replace
                       (thunk (graphvizOut tvar)))
        (send tvar setValue! t 'user #:forget true)))

(mathOpt (mathOpt (mathOpt '(= (- (+ (expt x 2) (* 3 x)) 2)
             (- (+ (- (+ (* 4 (expt x 2))
                         (* 12 (expt x 2)))
                      (* 3 x))
                   6)
                x)))))
;(mathOpt '(= 0 4))

#lang racket

(require "constraint.rkt")

(provide ConsoleRep)

(define ConsoleRep
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
      (and (send c hasValue?) (display `(,(send c getName) = ,(send c getValue))) (newline)))
    (public resolve reevaluate attach disconnect)))

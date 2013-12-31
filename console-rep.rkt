#lang racket

(require "connector.rkt")
(require "traversable.rkt")

(provide ConsoleRep)

(define ConsoleRep
  (class* object% (writable<%> ConnectorObserver Traverseable)
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

    (define (custom-write out)
      (display 'ConsoleRep out))

    (define (custom-display out)
      (send this custom-write out))
    (define (getName)
      "ConsoleRep")
    (define (connectorNames)
      '())
    (define (neighbors)
      (list c))

    (public custom-write custom-display)
    (public neighbors)
    (public connectorNames getName resolve reevaluate attach disconnect)))

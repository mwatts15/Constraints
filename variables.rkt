#lang racket
(require "constraint.rkt")
(provide (all-defined-out))

; should only be placed on the "outside" of the constraint network
(define Variable
  (class Connector
    (super-new)
    (define (set newval [setter 'user] #:forget [shouldForget #f])
      (when shouldForget
        (send this forgetValue! setter))
      (super set newval setter))
    (override (set setValue!))))

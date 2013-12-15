#lang racket
(require "constraint.rkt")
(provide (all-defined-out))

(define Variable
  (class Connector
    (super-new)

    (init-field [name (gensym)])

    (define (set newval [setter this])
      ;(display `(setting ,this with ,newval from ,setter))(newline)
      (and (eq? setter this)
           (send this forgetValue! this))
      (super set newval setter))

    (define (getMembers)
      (field-names this))
    (override (set setValue!))
    (public getMembers)))

(define ObjectV
  (class Variable
    (field [x (new Variable)]
           [y (new Variable)]
           [width (new Variable)]
           [height (new Variable)])
    (super-new)))

(define SquareV
  (class ObjectV
    (field [s (new Variable)])
    (super-new)))

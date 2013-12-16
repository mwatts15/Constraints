#lang racket
(require "constraint.rkt")
(provide (all-defined-out))

(define Variable
  (class Connector
    (super-new)
    (init [name (gensym)])
    (define _name name)

    (define (set newval [setter this])
      ;(display `(setting ,this with ,newval from ,setter))(newline)
      (and (send this informant? setter)
           (send this forgetValue! setter))
      (super set newval setter))

    (define (getName)
      _name)
    (define (getMembers)
      (field-names this))
    (override (set setValue!))
    (public getMembers getName)))

(define ObjectV
  (class Variable
    (field [x (new Variable)]
           [y (new Variable)]
           [width (new Variable)]
           [height (new Variable)])
    (define/public (setPos p)
      (send x setValue! (send p get-x))
      (send y setValue! (send p get-y)))
    (define/public (setWidth w)
      (send width setValue! w))
    (define/public (setHeight h)
      (send height setValue! h))
    (super-new)))

(define SquareV
  (class ObjectV
    (field [s (new Variable)])
    (super-new)))

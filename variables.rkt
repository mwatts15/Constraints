#lang racket
(require "constraint.rkt")
(provide (all-defined-out))

; should only be placed on the "outside" of the constraint network
(define Variable
  (class Connector
    (super-new)

    (define _members (make-hash))

    (define (set newval setter)
      ;(display `(setting ,this with ,newval from ,setter))(newline)
      (and (send this informant? setter)
           (send this forgetValue! setter))
      (super set newval setter))
    (define (getMember n)
      (hash-ref _members n))

    (define (memberNames)
      (dict-keys _members))

    (define (declareMembers names)
      (for ([n names])
        (hash-set! _members n (new Variable))))

    (override (set setValue!))
    (public-final memberNames getMember declareMembers)))
(define-syntax-rule (getMember o x)
  (send o getMember 'x))

(define ObjectV
  (class Variable
    (super-new)
    (inherit getMember declareMembers)
    (declareMembers '(x y w h))

    (define (setPos p)
      (send (getMember 'x) setValue! (send p get-x))
      (send (getMember 'x) setValue! (send p get-y)))

    (define (setWidth w)
      (send (getMember 'w) setValue! w))

    (define (setHeight h)
      (send (getMember 'h) setValue! h))
    (public setWidth setHeight setPos)))

(define SquareV
  (class ObjectV
    (super-new)
    (send this declareMembers '(s))))

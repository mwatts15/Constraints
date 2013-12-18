#lang racket

(require rackunit)
(require rackunit/text-ui)
(provide (all-defined-out)
         (all-from-out rackunit)
         (all-from-out rackunit/text-ui))
(require "constraint.rkt")

(define TestConnector-1
  (class object%
    (super-new)
    (init [value 'unset])
    (field [v value] [c false])
    (define (connect constraint)
      (set! c constraint))
    (define (hasValue?)
      (not (unset? v)))
    (define (getValue)
      v)
    (define (setValue! newval [setter #f])
      (if (unset? newval)
        #f
        (if (unset? v)
          (begin (set! v newval)
                 (and (not setter)
                      (send c resolve)))
          (if (not (eq? newval v))
            (raise (exn:contradiction v newval this))
            'nothing))))
    (define (go)
      (send c resolve))
    (public go connect hasValue? setValue! getValue)))


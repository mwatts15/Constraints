#lang racket

(require rackunit)
(require rackunit/text-ui)
(provide (all-defined-out)
         (all-from-out rackunit)
         (all-from-out rackunit/text-ui))
(require "connector.rkt"
         "unset.rkt"
         "permute.rkt")

(define TestConnector-1
  (class object%
    (super-new)
    (init [value unset])
    (field [v value] [c false])
    (define (connect constraint)
      (set! c constraint))
    (define (hasValue?)
      (not (unset? v)))
    (define (getValue)
      v)
    (define (setValue! newval setter)
      (when (is-set? newval)
        (if (unset? v)
          (begin (set! v newval)
                 (unless (eq? setter c)
                   (send c resolve)))
          (unless (equal? newval v)
            (raise (exn:contradiction v newval this))))))
            
    (define (forgetValue! setter)
      (set! v unset)
      (unless (eq? setter c)
        (send c reevaluate)))

    (define (go)
      (send c resolve))

    (public go connect hasValue? forgetValue! setValue! getValue)))

(define (setAllOrders bindings)
  (define myName (gensym 'sao))
  (for ([b (permute (dict->list bindings))])
    (for ([connector (in-dict-keys b)])
      (send connector forgetValue! myName))
    (for ([(connector value) (in-dict b)])
      (send connector setValue! value myName))))


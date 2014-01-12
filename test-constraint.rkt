#lang racket

(require "constraint.rkt"
         "test-util.rkt"
         "connector.rkt"
         "unset.rkt"
         (prefix-in V: "value.rkt"))
(require rackunit)
(require rackunit/text-ui)


(define TestConstraint
  (class Constraint
    (super-new)
    (define/augment (resolve)
      #t)))

(define TestConnector-2
  (class object%
    (super-new)
    (field [v #f] [c false])
    (define (setValue! . _)
      (set! v #t))
    (define (forgetValue! . _)
      (set! v #f))
    (define (hasValue?)
      v)
    (define (connect con)
      (set! c con))
    (define (go)
      (send c reevaluate))
    (public go connect hasValue? forgetValue! setValue!)))

(define tests
  (list
    (test-suite "Constant"
      (test-case "set on connect"
        (let ([constant (new Constant [value 'value])]
              [c (new TestConnector-2)])
          (connect c constant 'toSet)
          (check-true (send c hasValue?)))))
    (test-suite "Constraint"
      (test-case "forgets propagates"
        (let ([c1 (new TestConnector-2)]
              [c2 (new TestConnector-2)]
              [cc (new TestConstraint [ports '(aPort anotherPort)])])
          (send c1 setValue!)
          (send c2 setValue!)
          (connect c1 cc 'aPort)
          (connect c2 cc 'anotherPort)
          (send c1 go)
          (check-false (send c1 hasValue?))
          (check-false (send c2 hasValue?))))
      (test-case "no ports after init"
        (let ([c1 (new TestConnector-2)]
              [cc (new TestConstraint)])
          (check-exn exn:fail? (thunk (connect c1 cc 'aPort))))))))

(for ([t tests])
  (run-tests t 'verbose))

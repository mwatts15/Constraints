#lang racket

(require "constraint.rkt")
(require "test-util.rkt")
(require (prefix-in V: "value.rkt"))
(require rackunit)
(require rackunit/text-ui)


(define TestConstraint
  (class object%
    (super-new)
    (init connector)
    (init [value 'unset])
    (field [v value] [c connector])
    (send c connect this)
    (define/public (resolve)
      (let* ([pval (send c getValue)])
        (send c setValue! v this)))))

(define TestConstraint_resolved
  (class object%
    (super-new)
    (init connector)
    (field [v false] [c connector])
    (send c connect this)
    (define/public (isResolved?)
      v)
    (define/public (resolve)
      (set! v true))))

(define TestConstraintInherited
  (class Constraint
    (super-new)
    (define/override (resolve)
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
    (test-suite "Constraint"
      (test-case "forgets propagates"
                 (let ([c1 (new TestConnector-2)]
                       [c2 (new TestConnector-2)]
                       [cc (new TestConstraintInherited)])
                   (send c1 setValue!)
                   (send c2 setValue!)
                   (connect c1 cc 'aPort)
                   (connect c2 cc 'anotherPort)
                   (send c1 go)
                   (check-false (send c1 hasValue?))
                   (check-false (send c2 hasValue?)))))
    (test-suite "PredicateConstraint"
      (test-case "ValBox with good value"
                 (let* ([vb (new ValBox)]
                        [tc (new TestConnector-1 [value (new V:ValueBox)])]
                        [result (new TestConnector-1 [value true])])
                   (connect tc vb 'value)
                   (connect result vb 'result)
                   (check-not-exn (thunk (send tc go)))))
      (test-case "ValBox self-satisfies"
                 (let* ([vb (new ValBox)]
                        [tc (new TestConnector-1)]
                        [result (new TestConnector-1 [value true])])
                   (connect tc vb 'value)
                   (connect result vb 'result)
                   (send tc go)
                   (check-pred (lambda (x) (is-a? x V:ValueBox)) (send tc getValue))))
      (test-case "ValBox with bad value"
                 (let* ([vb (new ValBox)]
                        [tc (new TestConnector-1 [value 2])]
                        [result (new TestConnector-1 [value true])])
                   (connect tc vb 'value)
                   (connect result vb 'result)
                   (check-exn exn:contradiction? (thunk (send tc go))))))
    (test-suite "Connector"
      (test-case "multiple constraints on one connector"
                 (let* ([c (new Connector)]
                        [tc1 (new TestConstraint_resolved [connector c])]
                        [tc2 (new TestConstraint_resolved [connector c])]
                        [tc3 (new TestConstraint_resolved [connector c])])
                   (for ([x (list tc1 tc2 tc3)])
                     (check-false (send x isResolved?)))
                   (send c setValue! 5 'user)
                   (for ([x (list tc1 tc2 tc3)])
                     (check-true (send x isResolved?)))))
      (test-case "setValue! different values"
                 (let ([c (new Connector)])
                   (send c setValue! 2 'user)
                   (check-exn exn:contradiction? (thunk (send c setValue! 3 'user)))))
      (test-case "setValue! twice"
                 (let ([c (new Connector)])
                   (send c setValue! 2 'user)
                   (check-not-exn (thunk (send c setValue! 2 'user)))))
      (test-case "requestSetValue! on new"
                 (let ([c (new Connector)])
                   (send c requestSetValue! 2 'user)
                   (check-true (send c hasValue?))
                   (check-eq? 2 (send c getValue))))
      (test-case "requestSetValue! on previously set"
                 (let ([c (new Connector)])
                   (send c setValue! 2 'user)
                   (send c requestSetValue! 3 'other-user)
                   (check-eq? 3 (send c getValue))))
      (test-case "requestSetValue! contradiction"
                 (let* ([c (new Connector)]
                        [tc (new TestConstraint [value 3] [connector c])])
                   (check-exn exn:contradiction? (thunk (send c requestSetValue! 2 'user))))))))


(for ([t tests])
  (run-tests t 'verbose))

#lang racket

(require rackunit)
(require rackunit/text-ui)
(require "constraint.rkt")
(require "variables.rkt")

(define TestVar
  (class Variable
    (field [x (new Variable)]
           [y (new Variable)])
    (super-new)))

(define TestEqualConstraint
  (class Constraint
    (super-new (ports '(lhs rhs)))
    (inherit getPort)
    (define/override (resolve)
      (let ([lhs (getPort 'lhs)]
            [rhs (getPort 'rhs)])
        (cond [(send lhs hasValue?)
               (send rhs setValue! (send lhs getValue) this)]
              [(send rhs hasValue?)
               (send lhs setValue! (send rhs getValue) this)])))))

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

(define tests
  (list
    (test-suite "ObjectV"
      (test-case "check position"
                 (let ([o (new ObjectV)])
                   (let ([x (send (get-field x o) getValue)]
                         [y (send (get-field y o) getValue)])
                     (check-eq? x 'unset)
                     (check-eq? y 'unset)))))
    (test-suite "Variable"
      (test-case "with members"
                 (let ([v (new TestVar)])
                   (check-pred unset? (send (get-field x v) getValue))))
      (test-case "multiple constraints on one variable"
                 (let* ([c (new TestVar)]
                        [tc1 (new TestConstraint_resolved [connector c])]
                        [tc2 (new TestConstraint_resolved [connector c])]
                        [tc3 (new TestConstraint_resolved [connector c])])
                   (for ([x (list tc1 tc2 tc3)])
                     (check-false (send x isResolved?)))
                   (send c setValue! 5)
                   (for ([x (list tc1 tc2 tc3)])
                     (check-true (send x isResolved?))))))
    (test-suite "Constraint"
      (test-case "on members"
                 (let* ([v (new TestVar)]
                        [c (new TestEqualConstraint)])
                   (connect (get-field y v) c 'rhs)
                   (connect (get-field x v) c 'lhs)
                   (send (get-field y v) setValue! 2)
                   (check-eq? (send (get-field x v) getValue) 2))))
    ))
(for ([t tests])
  (run-tests t 'verbose))

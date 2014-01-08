#lang racket

(require "connector.rkt"
         "test-util.rkt"
         "unset.rkt")

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

(define TestConstraint
  (class object%
    (super-new)
    (init connector)
    (init [value unset])
    (field [v value] [c connector])
    (send c connect this)
    (define/public (resolve)
      (let* ([pval (send c getValue)])
        (send c setValue! v this)))))

(define tests 
  (list 
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
      (test-case "(hasValue? c) <=> (unset? (getValue c))"
        (let* ([c (new Connector)])
          (check-false (send c hasValue?)) 
          (check-true (unset? (send c getValue))) 

          (send c setValue! 'aValue 'user)
          (check-true (send c hasValue?)) 
          (check-false (unset? (send c getValue))) 

          (send c forgetValue! 'user)
          (check-false (send c hasValue?)) 
          (check-true (unset? (send c getValue)))))
      (test-case "requestSetValue! contradiction"
        (let* ([c (new Connector)]
               [tc (new TestConstraint [value 3] [connector c])])
          (check-exn exn:contradiction? (thunk (send c requestSetValue! 2 'user))))))))

(for ([t tests])
  (run-tests t 'verbose))

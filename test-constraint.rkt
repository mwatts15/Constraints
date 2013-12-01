#lang racket

(require "constraint.rkt")
(require (prefix-in V: "value.rkt"))
(require rackunit)
(require rackunit/text-ui)


(define (resolveNot)
  (define argument-options (list true false 'unset))
  (define (inner argVal outVal)
    ; |arg:Constant| ---> |myNot:Not| ---> |out:Constant|
    (define test-name (format "resolveNot (~a,~a)" argVal outVal))
    (test-case test-name
               (let ([myNot (new Not)]
                     [arg (new Constant)]
                     [out (new Constant)])
                 (connectConstraints myNot 'out out 'out)
                 (connectConstraints myNot 'arg arg 'out)
                 (send arg setValue! argVal)
                 ((if (and (eq? outVal argVal) (not (unset? outVal)))
                    (curry check-exn exn:contradiction?)
                    identity) (thunk (send out setValue! outVal))))))
  (for ([a argument-options])
    (for ([o argument-options])
      (inner a o))))

(define (resolveOr)
  (define argument-options (list true false 'unset))
  ; the order doesn't matter for the arguments
  (define setting-orders
    '((3 1 2) (1 2 3) (1 3 2) (3 2 1) (2 1 3) (2 3 1)))
  (define (inner arg1Val arg2Val outVal settingOrder)
    ; |arg:Constant| ---> |myNot:Not| ---> |out:Constant|
    (define test-name (format "resolveOr (~a,~a,~a)" arg1Val arg2Val outVal))
    (test-case test-name
               (let ([myOr (new Or)]
                     [arg1 (new Constant)]
                     [arg2 (new Constant)]
                     [out (new Constant)]
                     [shouldCheckExn? (or (and (or (eq? arg1Val true)
                                   (eq? arg2Val true))
                               (eq? outVal false))
                          (and (eq? arg1Val false)
                               (eq? arg2Val false)
                               (eq? outVal true)))])
                 (connectConstraints myOr 'out out 'out)
                 (connectConstraints myOr 'arg1 arg1 'out)
                 (connectConstraints myOr 'arg2 arg2 'out)
                 ((if shouldCheckExn?
                    (curry check-exn exn:contradiction?)
                    (lambda (f) (f)))
                  (thunk (for ([i settingOrder])
                    (case i
                      [(1) (send arg1 setValue! arg1Val)]
                      [(2) (send arg2 setValue! arg2Val)]
                      [(3) (send out setValue! outVal)])))))))
  (for ([a1 argument-options])
    (for ([a2 argument-options])
      (for ([o argument-options])
        (for ([so setting-orders])
          (inner a1 a2 o so))))))

(define (resolveAnd)
  (define argument-options (list true false 'unset))
  ; the order doesn't matter for the arguments
  (define setting-orders
    '((3 1 2) (1 2 3) (1 3 2) (3 2 1) (2 1 3) (2 3 1)))
  (define (inner arg1Val arg2Val outVal settingOrder)
    ; |arg:Constant| ---> |myNot:Not| ---> |out:Constant|
    (define test-name (format "resolveAnd (~a,~a,~a)" arg1Val arg2Val outVal))
    ;(display test-name)(newline)
    (test-case test-name
               (let ([myAnd (new And)]
                     [arg1 (new Constant)]
                     [arg2 (new Constant)]
                     [out (new Constant)]
                     [shouldCheckExn? 
                       (or (and (or (eq? arg1Val false) (eq? arg2Val false))
                            (eq? outVal true))
                           (and (and (eq? arg1Val true) 
                                     (eq? arg2Val true)
                                     (eq? outVal false))))])
                 (connectConstraints myAnd 'out out 'out)
                 (connectConstraints myAnd 'arg1 arg1 'out)
                 (connectConstraints myAnd 'arg2 arg2 'out)
                 ((if shouldCheckExn?
                    (curry check-exn exn:contradiction?)
                    (lambda (f) (f)))
                  (lambda () 
                    (for ([i settingOrder])
                      (case i
                        [(1) (send arg1 setValue! arg1Val)]
                        [(2) (send arg2 setValue! arg2Val)]
                        [(3) (send out setValue! outVal)])))))))
  (for ([a1 argument-options])
    (for ([a2 argument-options])
      (for ([o argument-options])
        (for ([so setting-orders])
          (inner a1 a2 o so))))))

(define TestConstraint
  (class object%
    (super-new)
    (init value connector)
    (field [v value] [c connector])
    (send c connect this)
    (define/public (resolve)
      (let* ([pval (send c getValue)])
        (send c setValue! v this)))))

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
    (define (setValue! newval _)
      (if (unset? v)
        (set! v newval)
        (if (not (eq? newval v))
          (raise (exn:contradiction v newval))
          'nothing)))
    (define (go)
      (send c resolve))
    (public go connect hasValue? setValue! getValue)))

(define tests
  (list
    (test-suite "Constraint"
      (resolveNot)
      (resolveOr)
      (resolveAnd))
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

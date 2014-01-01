#lang racket

(require "test-util.rkt"
         "bool.rkt"
         "constraint.rkt"
         "connector.rkt"
         "permute.rkt"
         "unset.rkt")

(define argument-options (list true false unset))
(define setting-orders
  '((3 1 2) (1 2 3) (1 3 2) (3 2 1) (2 1 3) (2 3 1)))

(define (resolveNot)
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

(define (resolveTest constraint opname op)
  (define (inner arg1Val arg2Val outVal settingOrder)
    ; |arg:Constant| ---> |myNot:Not| ---> |out:Constant|
    (define test-name (format "resolve~a (~a,~a,~a)" opname arg1Val arg2Val outVal))

    (define shouldCheckExn?
      (let ([l (map (lambda (x)
                      (if (unset? x) 
                        '(#t #f)
                        (list x)))
                    (list arg1Val arg2Val outVal))])
        (not (for*/or ([a1 (first l)]
                       [a2 (second l)]
                       [o (third l)])
                      (eq? (op a1 a2) o)))))
    (test-case test-name
               (let ([c (new constraint)]
                     [v1 (new TestConnector-1)]
                     [v2 (new TestConnector-1)]
                     [o (new TestConnector-1)])
                 (connect o c 'out)
                 (connect v1 c 'lhs)
                 (connect v2 c 'rhs)
                 ((if shouldCheckExn?
                    (curry check-exn exn:contradiction?)
                    (lambda (f) (f)))
                  (thunk (for ([i settingOrder])
                    (case i
                      [(1) (send v1 setValue! arg1Val 'tester)]
                      [(2) (send v2 setValue! arg2Val 'tester)]
                      [(3) (send o setValue! outVal 'tester)])))))))
  (for ([a1 argument-options])
    (for ([a2 argument-options])
      (for ([o argument-options])
        (for ([so setting-orders])
          (inner a1 a2 o so))))))


(define tests
  (list
    (test-suite "Bool"
                (resolveNot)
                (resolveTest Or "Or" (lambda (x y) (or x y)))
                (resolveTest And "And" (lambda (x y) (and x y))))))

(for ([t tests])
  (run-tests t 'verbose))

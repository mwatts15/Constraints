#lang racket

(require "test-util.rkt")
(require "constraint.rkt")

; product tests
(define (constraintTests size)
  (for/list ([op (list / + * -)])
    (for*/list ([i (in-range size)]
                [j (in-range size)])
      (with-handlers ([(lambda(_)#t) (lambda(_)(list i j 'undefined))])
        (list i j (op i j))))))
(define setting-orders
  '((3 1 2) (1 2 3) (1 3 2) (3 2 1) (2 1 3) (2 3 1)))
(define (resolveTest constraint opname op values)
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
                      [(1) (send v1 setValue! arg1Val)]
                      [(2) (send v2 setValue! arg2Val)]
                      [(3) (send o setValue! outVal)])))))))
  (for ([args values])
    (for ([so setting-orders])
      (apply inner (append args (list so))))))


;(define tests
  ;(list
    ;(test-suite "Product"
                ;(resolveTest Product "Product" * (constraintTests 2)


(for ([t tests])
  (run-tests t 'verbose))

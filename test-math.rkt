#lang racket

(require "test-util.rkt"
         "constraint.rkt"
         "unset.rkt"
         "permute.rkt"
         "connector.rkt"
         "math.rkt")

(define testValues
  (for*/list ([i (in-range -50 50)]
              [j (in-range -50 50)])
    (cons i j)))

(define (testMathResolveSuccess constraint name op)
  (test-suite name
    (test-case "success"
      (let ([c (new constraint)]
            [v1 (new TestConnector-1)]
            [v2 (new TestConnector-1)]
            [o (new TestConnector-1)])
        (connect o c 'res)
        (connect v1 c 'lhs)
        (connect v2 c 'rhs)
        (for ([(a b) (in-dict testValues)])
          (let ([res (if (and (eq? op /) 0) 
                       unset 
                       (op a b))])
            (test-not-exn (format "~a ~a" a b)
              (thunk (setAllOrders `((,v1 . ,a)
                                     (,v2 . ,b)
                                     (,o . ,res)))))))))))

(define tests
  (map testMathResolveSuccess
       (list Product Quotient Sum Difference)
       (list "Product" "Quotient" "Sum" "Difference")
       (list * / + -)))


(for ([t tests])
  (run-tests t 'verbose))

#lang racket

(require "value.rkt")
(require rackunit)
(require rackunit/text-ui)

(define tests
  (list
    (test-suite "Range"
                (test-case "Range addition"
                           (let ([r1 (new Range (start 2) (end 3))]
                                 [r2 (new Range (start 5) (end 7))])
                             (let ([r3 (send r1 add r2)])
                               (check-eq? 7 (get-field start r3))
                               (check-eq? 10 (get-field end r3))))))))

(for ([t tests])
  (run-tests t 'verbose))

#lang racket

(provide permute)

(define (permute lst)
  (cond
    [(= (length lst) 1) (list lst)]
    [else (apply append
                 (map (lambda (i)
                        (map (lambda (j)
                               (cons i j))
                             (permute (remove* (list i) lst))))
                      lst))]))



#lang racket
(require "console-rep.rkt")

(define e '((+ (lhs a) (rhs b) (res s1)) 
            (+ (lhs s1) (rhs c) (res s2)) 
            (+ (lhs s2) (rhs d) (res s3))
            (+ (lhs k) (rhs q) (res z))
            (/ (lhs p) (rhs q) (res quot))))
; simple syntax. give the name of the constraint (in general, the name of the 
; constraint class name but starting with a lowercase letter) followed by a list 
; of lists with the first element being the name of the port to attach to and
; the second element being the name of the variable in the formula. 
;
; ex: a constraint that the array, a, has an even element at 2
(define n1 '((even (arg v))
             (array (array a) (index i) (value v))))

(define (maine)
  (let* ([c (f->c k)])
    (for ([v (send c getConnectors)])
      (new ConsoleRep [c v]))
    (with-method ([s (c setPortValue!)])
      (s 'l 21 'user)
      (s 'k 25 'user)
      (s 'x 10 'user)
      (s 'y 10 'user))))


#lang racket

(require "constraint-lang.rkt"
         "math-base.rkt")
(provide (all-defined-out))

(define Difference
  (f->c 
    '((+ (res lhs) (lhs rhs) (rhs res)))
    `((+ . ,Sum))))

(define Quotient
  (f->c 
    '((* (res lhs) (lhs rhs) (rhs res)))
    `((* . ,Product))))


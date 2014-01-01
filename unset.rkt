#lang racket

(provide unset? is-set? unset)
(define unset 'nothing)
(define unset?
  (curry eq? unset))

(define is-set?
  (negate unset?))



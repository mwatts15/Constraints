#lang racket

(provide (all-defined-out))

(define Box
  (class object%
    (init [_initializer false])
    (if (is-a? _initializer Box)
      ; copy data
      #t
      #f)
    (super-new)))
(define ValueBox
  (class Box
    (init [value false])
    (if (is-a? value Box)
      (super-make-object value)
      (super-new))))


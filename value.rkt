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

(define Set
  (interface is-subset? is-member?))
    
  ; define a set interface
  ; define functions on sets
  ;   union, intersection, etc.
; A union of Sets
(define Union
  (class* (Set) object%
          [init
          (define my-sets '())
    (define (is-subset? a-set)
      (sequence-andmap (lambda (x) (ormap (cut is-member? <> x) my-sets))
              (send a-set values)))))

(define Range
  (class* (Set) object%
    (super-new)
    (init-field start end)
    (define/public (add other)
      (cond [(number? other)
             (add-number other)]
            [(is-a? other Range)
             (add-range other)]))

    (define (add-number number)
      (new Range [start (+ start number)] [end (+ end number)]))

    (define (add-range range)
      (new Range 
           [start (+ (get-field start range) start)]
           [end (+ (get-field end range) end)]))))
(define Class
  (class Set


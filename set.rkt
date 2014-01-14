#lang racket
(provide (all-defined-out))

(define (intersect s1 s2)
  (if (or (eq? s1 EmptySet)
          (eq? s2 EmptySet))
    EmptySet
    (match (list s1 s2)
       [(list (Range st1 en1) (Range st2 en2))
        (new Range (max st1 st2) (min en1 en2))])))

(define Set
  (class* object% (Value)
    (super-new)
    (abstract is-subset?
              intersect
              is-member?)
    (define (isConsistentWith? other) (is-subset? other))
    (public isConsistentWith?)))
    
(define Singleton
  (class Set
    (super-new)
    (init value)
    (define (is-subset? s)
      (or (equal? this s))
    (define (is-member? e)
      #f)
    (define (intersect other)
      this)
    (override is-subset?
              is-member?
              intersect)))
(define EmptySet
  (new (class Set
         (super-new)
         (define (is-subset? s)
           #f)
         (define (is-member? e)
           #f)
         (define (intersect other)
           this)
         (override is-subset?
                   is-member?
                   intersect))))
; define a set interface
; define functions on sets
;   union, intersection, etc.
; A union of Sets
(define Union
  (class Set
    [init sets]
    (super-new)
    (define my-sets '())
    (define (is-member? x)
      (ormap (lambda (y) (send y is-member? x)) my-sets))
    (define (is-subset? a-set)
      (sequence-andmap (lambda (x) (is-member? x)) (send a-set values)))
    (override is-member? is-subset?)))

(struct Range (start end))
; a closed interval
#|(define Range|#
  ;; defines a closed interval between _start and _end
  ;(class* Set (writable<%> equal<%>)
    ;(super-new)
    ;(init start end)
    ;(define _start start)
    ;(define _end end)

    ;(define (equal-to? other arg)
      ;(or (eq? other this)
          ;(and (is-subset? other)
               ;(send other is-subset? this))))

    ;(define (equal-hash-code-of arg)
      ;(* (expt 2 _start)
         ;(expt 3 _end)))

    ;(define (equal-secondary-hash-code-of arg)
      ;(* (bitwise-xor _start 123)
         ;(bitwise-xor _end 2363)))

    ;(define (add other)
      ;(cond [(number? other)
             ;(add-number other)]
            ;[(is-a? other Range)
             ;(add-range other)]))

    ;(define (custom-write out)
      ;(fprintf out "[~a, ~a]" _start _end))

    ;(define (custom-display out)
      ;(custom-write out))
    
    ;(define (bounds)
      ;(values _start _end))

    ;(define (add-number number)
      ;(new Range [start (+ _start number)] [end (+ _end number)]))

    ;(define (add-range range)
      ;(let-values ([(s e) (send range bounds)])
        ;(new Range [start (+ s _start)] [end (+ e _end)])))

    ;; should this return an EmptySet object when there's nothing inside?
    ;; should this return a Singleton when start = end?
    ;(define (intersect range)
      ;(let-values ([(s e) (send range bounds)])
        ;(if (equal? s e) 
        ;(new Range [start (max s _start)] [end (min e _end)])))

    ;(define (is-member? x)
      ;(and (< x _end) (> x _start)))

    ;(define (is-subset? s)
      ;(if (is-a? s Range)
        ;(let-values ([(s e) (send s bounds)])
          ;(and (<= _start s)
               ;(>= _end e)))
        ;false))

    ;(override is-subset? is-member?)
    ;(public custom-write custom-display)
    ;(public equal-to? equal-hash-code-of equal-secondary-hash-code-of)
    #|(public intersect bounds add-number add-range add)))|#



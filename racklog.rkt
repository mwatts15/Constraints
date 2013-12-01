#lang racket
(require racklog)
(require racket/math)
 
;(define %parent
  ;(%rel ()
        ;[('bob 'mary)]
        ;[('alice 'mary)]))
;(define %male 
  ;(%rel ()
        ;[('bob)]))
;(define %female 
  ;(%rel ()
        ;[('alice)]))

;(define %father
  ;(%rel (person child)
        ;[(person child)
         ;(%parent person child)
         ;(%male person)]))

;(define %mother
  ;(%rel (person child)
        ;[(person child)
         ;(%parent person child)
         ;(%female person)]))

; the predicate needs to have that whenever there isn't a box bound to the 
; symbol, a box gets created. Otherwise, we can look up the box with that
; symbol and get the right box.

(define boxes (make-hash))
(define (get-box logic-var)
  (hash-ref boxes logic-var #f))
(define (set-valBox logic-var)
  (hash-ref boxes logic-var))

(define (makeBox name)
  (if (dict-has-key? boxes name)
    name
    (begin
      (dict-set! boxes name (new box%))
      name)))

(define box%
  (class object%
    (super-new)
    [init-field (x 0) (y 0) (w 2) (h 2)]))

(define valBox%
  (class box%
    (super-new)
    [init (value 0)]
    [init-field (v value)]
    (define/public (set-value a-val)
      (set! v a-val))
    (define/public (has-value? a-val)
      (display "checking if valBox has value\n")
      (= a-val v))
    (define/public (get-value)
      (display "getting a value\n")
      v)))

; valBox will succeed if there is a valbox bound to the variable 
; or if the variable is unbound

(define %valBox
  (%rel (b)
    [((makeBox 'empty))]
    [(b) (%is #t (is-a? (get-box b) valBox%))]
    [((makeBox b))
     (%var b)]))

; sets the value if v is bound, sets v to the value otherwise
(define %hasValue
  (%rel (j v)
    [(j (send (get-box j) get-value)) (%valBox j) (%var v)]
    [(j v) (%valBox j) (%is #t #'(send (get-box j) has-value? v))]))

(define %car
  (%rel (x)
    [((cons x (_)) x)]))

(define %cdr
  (%rel (x)
    [('end _)]
    [((cons (_) x) x)]))

(define %boxing
  (%rel (x v xs)
    [('())]
    [((cons v xs))
     (%valBox x) (%hasValue x v) (%boxing x xs)]))

(define succ
  (lambda (x)
    (vector 'succ x)))

(define %add
  (%rel (x y z)
    [(0 y y)]
    [((succ x) y (succ z))
     (%add x y z)]))

(define jax
  (%which (k)
    (%car '(1 2 23 432 2hh) k)))

(define lax
  (%which (k)
    (%cdr '(a end) k)))

;(define spax
  ;(%which (t)
    ;(%boxing t '(1 2 23 432 5))))

(define bax
  (%which (x)
    (%hasValue x 'empty)))

(display bax)

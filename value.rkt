#lang racket

(require (only-in racket/draw point%))
(require errortrace)
(provide (all-defined-out))

(define Rectangle 
  (class object%
    (super-new)
    (field [w 2] [h 2])))

(define (make-point x y)
  (make-object point% x y))

(define Set
  (interface () is-subset? is-member?))
    
  ; define a set interface
  ; define functions on sets
  ;   union, intersection, etc.
; A union of Sets
(define Union
  (class* object% (Set)
    [init sets]
    (define my-sets '())
    (define (is-member? x)
      (ormap (lambda (y) (send y is-member? x)) my-sets))
    (define (is-subset? a-set)
      (sequence-andmap (lambda (x) (is-member? x)) (send a-set values)))
    (public is-member? is-subset?)))

(define Range
  (class* object% (Set) 
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
           [end (+ (get-field end range) end)]))
    (define (is-member? x)
      (and (< x end) (> x start)))
    (define (is-subset? s)
      (if (is-a? s Range)
        (and (< start (get-field start s))
             (> end (get-field end s)))
        false))

    (public add-number add-range is-subset? is-member?)))

(define World 
  (let ()
    (define-member-name get-o->l (generate-member-key))
    (define-member-name get-l->o (generate-member-key))
    (define emptyWorld
      (new (class object%
             (super-new)
             (define/public (get-o->l)
                            (hash))
             (define/public (get-l->o)
                            (hash)))))
    (class object%
      (super-new)
      (init [oldWorld emptyWorld])

      (define (get-o->l) _o->l)
      (define (get-l->o) _l->o)

      (define _o->l (send oldWorld get-o->l))

      (define _l->o (send oldWorld get-l->o))

      (define (placeObject o l)
        (if (dict-has-key? _o->l o)
          (moveObject o l)
          (begin (set! _o->l (dict-set _o->l o l))
                 (set! _l->o (dict-update _l->o l (lambda (x) (cons o x)) '())))))

      (define (getObjectsAt l)
        (dict-ref _l->o l '()))

      (define (removeObjectsAt l)
        (let ([objects (dict-ref _l->o l)])
          (set! _l->o (dict-remove _l->o l))
          (set! _o->l (for/fold ([res _o->l])
                                ([o objects])
                                (dict-remove res o)))
          objects))
      (define (removeObject o)
        (let* ([location (dict-ref _o->l o)]
               [objects (dict-ref _l->o location)])
          (set! _l->o (dict-update _l->o location (lambda (x) (remove o x))))
          (set! _o->l (dict-remove _o->l o))))

      (define (moveObject o newLocation)
        (removeObject o)
        (placeObject o newLocation))

      (public get-o->l
              get-l->o
              getObjectsAt
              removeObjectsAt
              moveObject
              placeObject))))
              

; add a point to a vector
(define (add/pv p v)
  (make-object point% (+ (send p get-x) (send v get-x))
               (+ (send p get-y) (send v get-y))))

(define (sub/pp p1 p2)
  (make-object point% (- (send p1 get-x) (send p2 get-x))
               (- (send p1 get-y) (send p2 get-y))))

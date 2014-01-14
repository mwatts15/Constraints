#lang racket

(require "unset.rkt"
         "set.rkt")
(provide (all-defined-out)
         (all-from-out "set.rkt"))

(define Value (interface () 
                         ; reflexive
                         isConsistentWith?))

(define Rectangle 
  (class* object% (Value)
    (super-new)
    (field [w 2] [h 2])
    (define/public (isConsistentWith? other)
      (and (eq? (get-field w other) w)
           (eq? (get-field h other) h)))))

(define Point 
  (class* object% (Value)
    (super-new)
    (init-field [x 0] [y 0])
    (define/public (get-x)
      x)
    (define/public (get-y)
      y)
    (define/public (isConsistentWith? other)
      (and (eq? (get-field x other) x)
           (eq? (get-field y other) y)))))

(define (make-point x y)
  (make-object Point x y))

(define World 
  (let ()
    (define-member-name get-o->l (generate-member-key))
    (define-member-name get-l->o (generate-member-key))
    (define-member-name get-consistencyWidget (generate-member-key))

    (define emptyWorld
      (new (class object%
             (super-new)
             (define/public (get-consistencyWidget) (gensym 'cw))
             (define/public (get-o->l) (hash))
             (define/public (get-l->o) (hash)))))

    (class* object% (Value)
      (super-new)
      (init [oldWorld emptyWorld])

      (define (get-o->l) _o->l)
      (define (get-l->o) _l->o)
      (define (get-consistencyWidget) _cw)


      (define _o->l (send oldWorld get-o->l))
      (define _l->o (send oldWorld get-l->o))
      (define _cw (send oldWorld get-consistencyWidget))

      (define (isConsistentWith? other)
        (and (is-a? other World)
             (eq? _cw (send other get-consistencyWidget))))

      (define (placeObject o l)
        (if (dict-has-key? _o->l o)
          (moveObject o l)
          (begin (set! _o->l (dict-set _o->l o l))
                 (set! _l->o (dict-update _l->o l (lambda (x) (cons o x)) '())))))

      (define (getLocationOf o)
        (dict-ref _o->l o unset))

      (define (getObjectsAt l)
        (dict-ref _l->o l '()))

      (define (removeObjectsAt l)
        (let ([objects (dict-ref _l->o l)])
          (set! _cw (gensym))
          (set! _l->o (dict-remove _l->o l))
          (set! _o->l (for/fold ([res _o->l])
                                ([o objects])
                                (dict-remove res o)))
          objects))
      (define (removeObject o)
        (let* ([location (dict-ref _o->l o)]
               [objects (dict-ref _l->o location)])
          (set! _cw (gensym))
          (set! _l->o (dict-update _l->o location (lambda (x) (remove o x))))
          (set! _o->l (dict-remove _o->l o))))

      (define (moveObject o newLocation)
        (unless (equal? (dict-ref _o->l o) newLocation)
          (removeObject o)
          (placeObject o newLocation)))

      (public get-o->l
              get-l->o
              get-consistencyWidget)
      (public getObjectsAt
              isConsistentWith?
              getLocationOf
              removeObjectsAt
              moveObject
              placeObject))))
              

; add a point to a vector
(define (add/pv p v)
  (make-point (+ (send p get-x) (send v get-x))
              (+ (send p get-y) (send v get-y))))

(define (sub/pp p1 p2)
  (make-point (- (send p1 get-x) (send p2 get-x))
              (- (send p1 get-y) (send p2 get-y))))

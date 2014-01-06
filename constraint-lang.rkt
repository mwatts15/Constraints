#lang racket

(require "constraint.rkt")
(require "connector.rkt")
(require "console-rep.rkt")
(require "math.rkt")
; the basic format is a list of constraints, which can be thought of as
; a conjunction

(define (f->c formulas)
  (define external
    (class Constraint
      [init vstore]
      (define _vs vstore)
      (super-new [ports (dict-keys vstore)] [name 'Formula])
      (for ([(k v) _vs])
        (send this attach k v))))

  (define vstore (make-hash))
  (define (getVar v)
    (dict-ref! vstore v (lambda ()
                          (new Connector [name v]))))
  (define _ops ; indexed by number of operands
    (hash '+ Sum
          'neg Negation
          '- Difference
          '/ Quotient
          '= Equal
          '* Product
          'array Array
          'square Square
          'even Even
          'list List
          'point Point
          'at At))

  (for ([f formulas])
    (let* ([c (new (dict-ref _ops (first f)))]
           [connectors (send c connectorNames)])
      (for ([v (map first (rest f))])
        (unless (member v connectors)
          (error (format "not a port for ~a: ~a" c v))))
      (for ([arg (rest f)])
        (let ([connector (getVar (second arg))]
              [port (first arg)])
          (connect connector c port)))))
  (new external [vstore vstore]))

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

(define k '((square (ob s) (side l))
            (square (ob r) (side k))
            (at (loc p) (ob s) (world w))
            (point (pt p) (x x) (y y))
            (+ (lhs l) (rhs x) (res qx))
            (point (pt q) (x qx) (y y))
            (at (loc q) (ob r) (world w))))
(define (maine)
  (let* ([c (f->c k)])
    (for ([v (send c getConnectors)])
      (new ConsoleRep [c v]))
    (with-method ([s (c setPortValue!)])
      (s 'l 21 'user)
      (s 'k 25 'user)
      (s 'x 10 'user)
      (s 'y 10 'user))))
  
  ;(let* ([c (f->c e)])
    ;(for ([v (send c getConnectors)])
      ;(new ConsoleRep [c v]))
    ;(with-method ([s (c setPortValue!)])
      ;(s 'k 1 'user)
      ;(s 'quot 7/4 'user)
      ;(s 'z 5 'user))))
(maine)
(provide (all-defined-out))

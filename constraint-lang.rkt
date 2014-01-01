#lang racket

(require "constraint.rkt")
(require "connector.rkt")
(require "console-rep.rkt")
(require "math.rkt")
; the basic format is a list of constraints, which can be thought of as
; a conjunction
;(define LeftOf
  ;(class Constraint
    ;(super-new (ports '(o1 o2 out)))
    ;(inherit getPort)
    ;(define/override (attach p c)
      ;(and (or (eq? p 'o1) (eq? p 'o2))
           ;(connect (get-field x c) this `(,p x))
           ;(connect (get-field width c) this `(,p width)))
      ;(super attach p c))

    ;(define/override (resolve)
        ;(let ([o1x (getPort '(o1 x))]
              ;[o1w (getPort '(o1 width))]
              ;[o2x (getPort '(o2 x))]
              ;[r (getPort 'out)])
          ;(and (and (send r hasValue?) (eq? true (send r getValue)))
               ;(cond [(and (send o1x hasValue?) (send o1w hasValue?)) 
                      ;(send o2x setValue! (+ (send o1x getValue) (send o1w getValue)) this)]
                     ;[(and (send o2x hasValue?) (send o1w hasValue?))
                      ;(send o1x setValue! (- (send o2x getValue) (send o1w getValue)) this)]))))))

(define (f->c formulas [ops '()])
  (define _ops ; indexed by number of operands
    (apply hash 
           '+ Sum
           'neg Negation
           '- Difference
           '/ Quotient
           '= Equal
           '* Product
           'array Array
           'square Square
           'even Even
           'list List
           ops))

  (define external
    (class Constraint
      [init vstore]
      (define _vs vstore)
      (super-new [ports (dict-keys vstore)] [name 'Formula])
      (for ([(k v) _vs])
        (send this attach k v))))

  (define vstore (make-hash))
  (define (getVar v)
    (dict-ref! vstore v (lambda () (new Connector [name v]))))

  (for ([f formulas])
    (let ([c (new (dict-ref _ops (first f)))])
      (for ([arg (rest f)])
        (let ([connector (getVar (second arg))]
              [port (first arg)])
          (connect connector c port)))))
  (new external [vstore vstore]))

(define e '((+ (lhs a) (rhs b) (sum s1)) 
            (+ (lhs s1) (rhs c) (sum s2)) 
            (+ (lhs s2) (rhs d) (sum s3))
            (+ (lhs k) (rhs q) (sum z))
            (/ (lhs p) (rhs q) (quotient quot))))
; simple syntax. give the name of the constraint (in general, the name of the 
; constraint class name but starting with a lowercase letter) followed by a list 
; of lists with the first element being the name of the port to attach to and
; the second element being the name of the variable in the formula. 
;
; ex: a constraint that the array, a, has an even element at 2
(define n1 '((even (arg v))
             (array (array a) (index i) (value v))))
(define k '((square (ob s) (side l))
            (at (loc p) (ob s))
            (point (pt p) (x x) (y y))
            (+ (lhs l) (rhs x) (sum qx))
            (point (pt q) (x qx) (y qy))
            (at (loc q) (ob r))))
(let* ([c (f->c k)])
  (for ([v (send c getConnectors)])
    (new ConsoleRep [c v]))
  (send c setPortValue! 'l 21 'user))
(let* ([c (f->c e)])
  (for ([v (send c getConnectors)])
    (new ConsoleRep [c v]))
  (with-method ([s (c setPortValue!)])
    (s 'k 1 'user)
    (s 'quot 7/4 'user)
    (s 'z 5 'user)))

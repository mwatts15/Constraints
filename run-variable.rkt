#lang racket

(require "constraint.rkt")
(require "variables.rkt")

; can constraints make new constraints on their members in the process of resolution?
; are there any dangers to the system's correctness that way?
;
; I think not. The system is correct as long as all of the constraints have their "ports"
; connected to something and as long as we the relevant constraints to forget their values 
; before we assign a new one.

; asserts that one object (o1) is directly to the left of the other (o2)
; don't check some other things like whether or not the objects 
; are actually objects. presumably, we'll have an entailment phase 
; that adds in constraints to specify things like that
;
; constraints which use member variables of a variable must manually connect them
; in an augmented 
(define LeftOf
  (class Constraint
    (super-new (ports '(o1 o2 out)))
    (inherit getPort)
    (define/override (attach p c)
      (and (or (eq? p 'o1) (eq? p 'o2))
           (connect (get-field x c) this `(,p x))
           (connect (get-field width c) this `(,p width)))
      (super attach p c))

    (define/override (resolve)
        (let ([o1x (getPort '(o1 x))]
              [o1w (getPort '(o1 width))]
              [o2x (getPort '(o2 x))]
              [r (getPort 'out)])
          (and (and (send r hasValue?) (eq? true (send r getValue)))
               (cond [(and (send o1x hasValue?) (send o1w hasValue?)) 
                      (send o2x setValue! (+ (send o1x getValue) (send o1w getValue)) this)]
                     [(and (send o2x hasValue?) (send o1w hasValue?))
                      (send o1x setValue! (- (send o2x getValue) (send o1w getValue)) this)]))))))

; We have observers that get notified when a variable changes.
; These are similar to the probes in the SICP formulation.
; However, our observers can be associated with more than one
; variable. The associations are set up by an intermediary that
; performs the connections to the variables. In this case, the
; probe itself sets up the connections to the other things.
; Although observers have an interface that matches constraints
; they are not constraints.
(define SquareRep
  (class object%
    (super-new)
    (init-field connector)
    (connect (get-field x connector) this)
    (connect (get-field y connector) this)
    (connect (get-field width connector) this)
    (define (resolve)
      (printf "~a (~a,~a) w=~a ~n"
              (send connector getName)
              (send (get-field x connector) getValue)
              (send (get-field y connector) getValue)
              (send (get-field width connector) getValue)))

    (define (reevaluate) (resolve))

    (define (disconnect port)
      (error "nope"))

    (define (attach port c)
      void)

    (public resolve reevaluate 
            disconnect attach)))

(let* ([o1 (new SquareV)]
       [o2 (new SquareV)]
       [t (new Variable)]
       [l (new LeftOf)]
       [sr1 (make-object SquareRep o1)]
       [sr2 (make-object SquareRep o2)])
  (connect o1 l 'o1) 
  (connect o2 l 'o2) 
  (connect t l 'out)
  (send (get-field width o1) setValue! 12)
  (send (get-field x o1) setValue! 12)
  (send t setValue! #t)
  (send (get-field width o1) setValue! 17))

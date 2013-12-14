#lang racket

(require prelude)
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
      (display 'resolving)(newline)
        (let ([o1x (getPort '(o1 x))]
              [o1w (getPort '(o1 width))]
              [o2x (getPort '(o2 x))]
              [r (getPort 'out)])
          (display (send r hasValue?)) (newline)
          (and (and (send r hasValue?) (eq? true (send r getValue)))
               (cond [(and (send o1x hasValue?) (send o1w hasValue?)) 
                      (display 'here) (newline)
                      (send o2x setValue! (+ (send o1x getValue) (send o1w getValue)) this)]
                     [(and (send o2x hasValue?) (send o1w hasValue?))
                      (display 'there) (newline)
                      (send o1x setValue! (- (send o2x getValue) (send o1w getValue)) this)]))))))

(let ([o1 (new SquareV)]
      [o2 (new SquareV)]
      [t (new Variable)]
      [l (new LeftOf)])
  (connect o1 l 'o1) 
  (connect o2 l 'o2) 
  (connect t l 'out)
  (send (get-field width o1) setValue! 12)
  (send (get-field x o1) setValue! 12)
  (send t setValue! #t)
  (send (get-field width o1) setValue! 17)
  (printf "o1 (~a,~a) w=~a ~n"
          (send (get-field x o1) getValue)
          (send (get-field y o1) getValue)
          (send (get-field width o1) getValue))
  (printf "o2 (~a,~a) w=~a ~n"
          (send (get-field x o2) getValue)
          (send (get-field y o2) getValue)
          (send (get-field width o2) getValue)))

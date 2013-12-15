#lang racket
(require (prefix-in V: "value.rkt"))
(provide (all-defined-out))

(define unset?
  (curry eq? 'unset))
(define Constraint
  (class* object% (writable<%>)
    (super-new)
    [init (ports '())]
    (field 
      [connectors (make-hash (map (lambda (l) (cons l 'unset))
                                   ports))])
    (define (custom-write out)
      (display this% out)
      (write (dict-keys connectors) out))

    (define (custom-display out)
      (send this custom-write out))
    ; Try to resolve
    ; return If the constraints were satisfied, true
    ;        otherwise false
    (define (resolve)
      (error "abstract"))

    (define (reevaluate)
      (for ([(_ c) connectors])
        (send c forgetValue! this))
      (send this resolve))

    (define (disconnect port)
      (hash-set! connectors port 'unset))

    (define (attach port connector)
      (hash-set! connectors port connector))

    (define (getPort port-name)
      (hash-ref connectors port-name))

    (public getPort 
            resolve reevaluate 
            disconnect attach)
    ; for writeable<%>
    (public custom-write custom-display)))

(define Not
  (class Constraint
    (super-new (ports '(arg out)))
    (inherit getPort)
    (define/override (resolve)
      (let ([arg (getPort 'arg)]
            [out (getPort 'out)])
        (if (or (send arg hasValue?) (send out hasValue?))
          (cond [(send arg hasValue?)
                 (send out setValue! (not (send arg getValue)) this)]
                [(send out hasValue?)
                 (send arg setValue! (not (send out getValue)) this)])
          false)))))

(define Or
  (class Constraint
    (super-new (ports '(arg1 arg2 out)))
    (inherit getPort)

    ; resolves the value at the given port if possible
    (define/override (resolve)
      (let ([a1 (getPort 'arg1)]
            [a2 (getPort 'arg2)]
            [o (getPort 'out)]
            [v1 (send (getPort 'arg1) getValue)]
            [v2 (send (getPort 'arg2) getValue)]
            [vo (send (getPort 'out) getValue)])
        (cond [(and (send a1 hasValue?) (send a2 hasValue?))
               (send o setValue! (or v1 v2) this)]
              [(or (and (send a1 hasValue?) (eq? true v1))
                   (and (send a2 hasValue?) (eq? true v2)))
               (send o setValue! true this)]
              [(and (send o hasValue?) (eq? false vo))
               (send a1 setValue! false this)
               (send a2 setValue! false this)]
              [(and (send a1 hasValue?) (send o hasValue?) (not v1))
               (send a2 setValue! vo this)]
              [(and (send a2 hasValue?) (send o hasValue?) (not v2))
               (send a1 setValue! vo this)])))))

(define And
  (class Constraint
    (super-new (ports '(arg1 arg2 out)))
    (inherit getPort)

    ; resolves the value at the given port if possible
    (define/override (resolve)
      ;(display "calling resolve for And")(newline)
      (let ([a1 (getPort 'arg1)]
            [a2 (getPort 'arg2)]
            [o (getPort 'out)]
            [v1 (send (getPort 'arg1) getValue)]
            [v2 (send (getPort 'arg2) getValue)]
            [vo (send (getPort 'out) getValue)])
        (cond [(and (send a1 hasValue?) (send a2 hasValue?))
               (send o setValue! (and v1 v2) this)]
              [(or (and (send a1 hasValue?) (eq? false v1))
                   (and (send a2 hasValue?) (eq? false v2)))
               (send o setValue! false this)]
              [(and (send o hasValue?) (eq? true vo))
               (send a1 setValue! true this)
               (send a2 setValue! true this)]
              [(and (send a1 hasValue?)
                    (send o hasValue?) 
                    (not (unset? vo))
                    (eq? false v1))
               (send a2 setValue! vo this)]
              [(and (send a2 hasValue?) 
                    (send o hasValue?) 
                    (not (unset? vo))
                    (eq? true v2))
               (send a1 setValue! vo this)]
              [else 'nothing-to-do])))))

(define-syntax-rule (PredicateConstraint name pred? satisfier)
  (define name
    (class Constraint
      (super-new (ports '(value result)))
      (inherit getPort)

      (define/override (resolve)
        (let* ([r (getPort 'result)]
               [p (getPort 'value)]
               [rval (send r getValue)]
               [pval (send p getValue)])
          (if (send p hasValue?)
            (if (pred? pval)
              (send r setValue! true this)
              (send r setValue! false this))
            (send p setValue! (satisfier pval) this)))))))

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

(PredicateConstraint ValBox 
                     (lambda (x) (is-a? x V:ValueBox)) 
                     (lambda (x) (new V:ValueBox [value x])))
(PredicateConstraint Box (lambda (x) (is-a? x V:Box)) thunk)
(PredicateConstraint Even even? thunk)
(PredicateConstraint Odd odd? thunk)
                
(define (exn:contradiction actual expected)
  (list 'contradiction expected actual))
(define (exn:contradiction? maybe-exn)
  (if (list? maybe-exn)
    (eq? 'contradiction (first maybe-exn))
    false))

(define Connector
  (class object%
    (super-new)
    (define v 'unset)
    (define informant false)
    (field [constraints '()])
    (define (hasValue?)
      (if informant true false))
    (define (set newval setter)
      ;(printf "~a setting ~a to ~a~n" setter this newval)
      (cond [(not (send this hasValue?))
             (set! v newval)
             (set! informant setter)
             (for ([x constraints]
                   #:unless (eq? x setter))
                  ;(display `(resolve ,x))
               (send x resolve))]
            [((compose not eq?) v newval)
             (raise (exn:contradiction newval v))]))

    (define (requestSetValue! newval setter)
      (cond [(send this hasValue?)
             (forget informant)
             (set newval setter)]
            [else (set newval setter)]))

    (define (forget retractor)
      (and (eq? retractor informant)
           (set! informant false)
           (for ([x constraints]
                 #:unless (eq? x retractor))
             (send x reevaluate))))

    (define (getValue)
      v)

    (define (connect new-constraint)
      ;(display `(connecting ,new-constraint to ,this)) (newline)
      (and (not (memq new-constraint constraints))
           ;(display 'here) (newline)
           (set! constraints (cons new-constraint constraints))))
           ;(display constraints) (newline)))

    (define (getConstraints)
      constraints)

    (public
      (forget forgetValue!)
      (set setValue!)
      requestSetValue!
      getConstraints
      hasValue?
      connect
      getValue)))

; holds a constant and matches the value of its sole connector.
; change the value though the methods setValue! and forgetValue!
(define Constant
  (class Constraint
    (super-new (ports '(out)))
    (inherit getPort)
    (init [value 'unset])
    (define v value)

    (define (set newval)
      (send (getPort 'out) forgetValue! this)
      (and (not (eq? newval 'unset))
           (begin (set! v newval)
                  (send (getPort 'out) setValue! v this))))

    (define (getValue)
      v)

    (define/override (resolve)
      (let* ([p (getPort 'out)])
        (and (not (unset? v))
             (send p setValue! v))))

    (public
      (set setValue!)
      getValue)))

(define (connect connector constraint [p null])
    (send connector
          connect constraint)
    (send constraint
          attach p connector))

(define (connectConstraints c1 port1 c2 port2)
  (define the-connector
    (let ([conn1 (send c1 getPort port1)]
          [conn2 (send c2 getPort port2)])
      (cond [(and (unset? conn1) (unset? conn2))
             (new Connector)]
            [(unset? conn1) conn2]
            [(unset? conn2) conn1]
            [else 'bad-connect])))
  (if (not (eq? the-connector 'bad-connect))
    (begin (send the-connector connect c1)
           (send the-connector connect c2)
           (send c1 attach port1 the-connector)
           (send c2 attach port2 the-connector))
    #f))

(define (disconnectConstraint c port)
  (send c disconnect port))

; constraints have connectors
; constraints constrain the cells they are connected to
; cells hold values
; constraints are satisfied by the assignment of values to cells
; cells can satisfy themselves
;

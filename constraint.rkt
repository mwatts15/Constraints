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
    (define (connect port connector)
      (hash-set! connectors port connector))
    (define (getPort port-name)
      (hash-ref connectors port-name))
    (public getPort resolve reevaluate disconnect connect)
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
    (define constraints '())
    (define (hasValue?)
      (if informant true false))
    (define (set newval setter)
      ;(printf "setting connector<<~a>> to ~a~n" constraints newval)
      (cond [(not (send this hasValue?))
             (set! v newval)
             (set! informant setter)
             (for ([x constraints]
                   #:unless (eq? x setter))
               (send x resolve))]
            [((compose not eq?) v newval)
             (raise (exn:contradiction newval v))]))

    (define (requestSetValue! newval setter)
      (cond [(send this hasValue?)
             (forget informant)
             (set newval setter)]
            [else (set newval setter)]))

    (define (forget retractor)
      (if (eq? retractor informant)
        
        (begin ;(printf "setting connector<<~a>> to forget~n" constraints)
               (set! informant false)
               (for ([x constraints]
                     #:unless retractor)
                 (send x reevaluate)))
        false))

    (define (getValue)
      v)

    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
        (set! constraints 
          (cons new-constraint constraints))
        false)
      (if (send this hasValue?)
        (send new-constraint resolve)
        false))

    (public
      (forget forgetValue!)
      (set setValue!)
      requestSetValue!
      hasValue?
      connect
      getValue)))

; holds a value and matches the value of its sole connector.
; change the value though the methods setValue! and forgetValue!
(define Variable
  (class Constraint
    (super-new (ports '(out)))
    (inherit getPort)

    (init-field [name (gensym)])
    (define v 'unset)

    (define (set newval)
      (if (eq? newval 'unset)
        (send this forgetValue!)
        (begin (set! v newval)
               (send (getPort 'out) setValue! v this)))
      (printf "~a = ~a~n" name v))

    (define (forget)
      (set! v 'unset)
      (send (getPort 'out) forgetValue! this)
      (printf "~a = ~a~n" name v))

    (define (getValue)
      (set! v (send (getPort 'out) getValue))
      v)

    (define/override (resolve)
      (let* ([p (getPort 'out)]
             [pval (send p getValue)])
        (and (send p hasValue?)
          (set! v pval)))
      (printf "~a = ~a~n" name v))

    (public
      (forget forgetValue!)
      (set setValue!)
      getValue)))
; holds a constant and matches the value of its sole connector.
; change the value though the methods setValue! and forgetValue!
(define Constant
  (class Constraint
    (super-new (ports '(out)))
    (inherit getPort)

    (define v 'unset)

    (define (set newval)
      ;(printf "setting ~a to ~a ~n" this newval)
      (if (eq? newval 'unset)
        (send this forgetValue!)
        (begin (set! v newval)
               (send (getPort 'out) setValue! v this))))

    (define (forget)
      (set! v 'unset)
      (send (getPort 'out) forgetValue! this))

    (define (getValue)
      (set! v (send (getPort 'out) getValue))
      v)

    (define/override (resolve)
      (let* ([p (getPort 'out)]
             [pval (send p getValue)])
        (if (and (send p hasValue?) (unset? v))
          (set! v pval)
          (send p setValue! v))))

    (public
      (forget forgetValue!)
      (set setValue!)
      getValue)))

(define (connect connector constraint p)
    (send connector
          connect constraint)
    (send constraint
          connect p connector))

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
           (send c1 connect port1 the-connector)
           (send c2 connect port2 the-connector))
    #f))

(define (disconnectConstraint c port)
  (send c disconnect port))

; constraints have connectors
; constraints constrain the cells they are connected to
; cells hold values
; constraints are satisfied by the assignment of values to cells
; cells can satisfy themselves
;

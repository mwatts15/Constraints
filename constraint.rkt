#lang racket

(require (prefix-in V: "value.rkt"))
(provide (all-defined-out))

(define unset?
  (curry eq? 'unset))
(define is-set?
  (negate unset?))
(define Constraint
  (class* object% (writable<%>)
    (super-new)
    [init (ports '())]
    (define connectors (make-hash (map (lambda (l) (cons l 'unset))
                                       ports)))
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
           (and (object-method-arity-includes?
                  c 
                  'getName 0)
                (display `(telling ,(send c getName) to forget. forget. forget...))
                (newline))
        (send c forgetValue! this))
      (send this resolve))

    (define (disconnect port)
      (hash-set! connectors port 'unset))

    (define (attach port connector)
      (hash-set! connectors port connector))

    (define (getPort port-name)
      (hash-ref connectors port-name))
    (define (connectorNames)
      (dict-keys connectors))

    (public getPort 
            resolve reevaluate 
            connectorNames
            disconnect attach)
    ; for writeable<%>
    (public custom-write custom-display)))

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

(define List
  (class Constraint
    (super-new (ports '(head tail list)))
    (inherit getPort)
    (define/override (resolve)
      (let ([o (getPort 'list)]
            [t (getPort 'tail)]
            [f (getPort 'head)])
        (let ([ov (send o getValue)]
              [tv (send t getValue)]
              [fv (send f getValue)])
          (cond [(and (is-set? tv)
                      (is-set? fv))
                 (send o setValue! (cons fv tv) this)]
                [(is-set? ov)
                 (send t setValue! (rest ov) this)
                 (send f setValue! (first ov) this)]))))))
(define Array
  (class Constraint
    (super-new (ports '(index value array)))
    (inherit getPort)
    (define/override (resolve)
      (let ([a (getPort 'array)]
            [i (getPort 'index)]
            [v (getPort 'value)])
        (let ([av (send a getValue)]
              [iv (send i getValue)]
              [vv (send v getValue)])
          (cond [(and (is-set? vv)
                      (is-set? iv)
                      (is-set? av))
                 (let ([vec (send a getValue)])
                   (vector-set! vec iv vv) 
                   (send a setValue! vec this))]
                [(and (is-set? av)
                      (is-set? iv))
                 (let ([vec (send a getValue)])
                   (send v setValue! (vector-ref vec iv) this))]
                [(and (is-set? av)
                      (is-set? vv))
                 (let ([vec (send a getValue)])
                   (send i setValue! (vector-member vv vec) this))]))))))

                
(define (exn:contradiction actual expected)
  (list 'contradiction ': 'expected expected 'got actual))
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
    (define (informant? c)
      (eq? c informant))

    (define (requestSetValue! newval setter)
      (display `(attempting to switch ,v to ,newval))(newline)
      (cond [(send this hasValue?)
             (forget informant)
             (set newval setter)]
            [else (set newval setter)]))

    (define (forget retractor)
      (and (eq? retractor informant)
           (set! informant false)
           (for ([x constraints]
                 #:unless (eq? x retractor))
             (display `(reevaluating ,x))(newline)
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
      informant?
      getValue)))

; holds a constant and matches the value of its sole connector.
; change the value though the methods setValue! and forgetValue!
(define Constant
  (class Constraint
    (super-new (ports '(out)))
    (inherit getPort)
    (define v 'unset)

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
             (send p setValue! v this))))

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

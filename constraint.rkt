#lang racket

(provide (all-defined-out))

(define unset?
  (curry eq? 'unset))
(define is-set?
  (negate unset?))
(define Constraint
  (class* object% (writable<%>)
    (super-new)
    (init (ports '()))
    (init-field [name 'GenericConstraint])
    (define connectors (make-hash (map (lambda (l) (cons l 'unset))
                                       ports)))
    (define (custom-write out)
      (display name out))

    (define (custom-display out)
      (send this custom-write out))
    ; Try to resolve
    ; return If the constraints were satisfied, true
    ;        otherwise false
    (define (resolve)
      (error "abstract"))

    (define (reevaluate)
      (for ([(k c) connectors])
        ;(display k)(newline)
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
(define Square
  (class Constraint
    (super-new (ports '(side rectangle)))
    (inherit getPort)
    (define/override (resolve)
      (let* ([s (getPort 'side)]
             [r (getPort 'rectangle)]
             [sv (send s getValue)]
             [rv (send r getValue)])
        (cond [(is-set? sv)
               (set-field! w rv sv)
               (set-field! h rv sv)
               (send r setValue! rv)]
              [(is-set? rv)
               (let ([w (get-field w rv)])
                 (set-field! h rv w)
                 (send s setValue! w)
                 (send r setValue! rv))])))))

(define (exn:contradiction oldval newval connector)
  (list 'contradiction ': 'setting newval 'from oldval 'on connector))

(define (exn:contradiction? maybe-exn)
  (if (list? maybe-exn)
    (eq? 'contradiction (first maybe-exn))
    false))

(define Connector
  (class* object% (writable<%>)
    (super-new)
    (init [name (gensym)])
    (define _name name)
    (define v 'unset)
    (define informant false)
    (define constraints '())
    (define (hasValue?)
      (if informant true false))

    (define (custom-write out)
      (display _name out)(display constraints out))

    (define (custom-display out)
      (send this custom-write out))

    (define (set newval setter)
      ;(display `(,setter settting ,this to ,newval from ,v))(newline)
      (cond [(not (send this hasValue?))
             (set! v newval)
             (set! informant setter)
             (for ([x constraints]
                   #:unless (eq? x setter))
               (send x resolve))]
            [((compose not eq?) v newval)
             (raise (exn:contradiction v newval this))]))
    (define (informant? c)
      (eq? c informant))

    (define (getName)
      _name)

    (define (requestSetValue! newval setter)
      ;(display `(attempting to switch ,v to ,newval))(newline)
      (cond [(send this hasValue?)
             (forget informant)
             (set newval setter)]
            [else (set newval setter)]))

    (define (forget retractor)
      (when (eq? retractor informant)
           ;(display `(,retractor retracting on ,this))(newline)
           (set! informant false)
           (set! v 'unset)
           (for ([x constraints]
                 #:unless (eq? x retractor))
             (send x reevaluate))))

    (define (getValue)
      v)

    (define (connect new-constraint)
      ;(display `(connecting ,new-constraint to ,this)) (newline)
      (and (not (memq new-constraint constraints))
           (set! constraints (cons new-constraint constraints))))

    (define (getConstraints)
      constraints)

    (public custom-write custom-display)
    (public
      (forget forgetValue!)
      (set setValue!)
      requestSetValue!
      getConstraints
      hasValue?
      connect
      informant?
      getName
      getValue)))

(define ConnectorObserver
  (class object%
    (define (attach . _) #t)
    (define (resolve . _) #t)
    (define (reevaluate . _) #t)
    (public attach resolve reevaluate)))

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

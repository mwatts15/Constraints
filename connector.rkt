#lang racket

(require "traversable.rkt"
         "value.rkt"
         "unset.rkt")

(provide (all-defined-out))

(define ConnectorObserver
  (interface ()
    attach resolve reevaluate))

(define Connector
  (class* object% (writable<%> Traverseable)
    (super-new)
    (init [name (gensym)])
    (define _name name)
    (define v unset)
    (define informant false)
    (define _observers (set))
    (define (hasValue?)
      (if informant true false))

    (define (custom-write out)
      (display _name out)(display _observers out))

    (define (custom-display out)
      (send this custom-write out))

    (define (_set newval setter)
      (display `(,setter settting ,this to ,newval from ,v))(newline)
      (cond [(not (send this hasValue?))
             (set! v newval)
             (set! informant setter)
             (for ([x _observers]
                   #:unless (eq? x setter))
               (send x resolve))]
            [((compose not consistent?) v newval)
             (raise (exn:contradiction v newval this))]))

    (define (informant? c)
      (eq? c informant))

    (define (getName)
      _name)

    (define (requestSetValue! newval setter)
      ;(display `(attempting to switch ,v to ,newval))(newline)
      (when (send this hasValue?)
        (forget informant))
      (_set newval setter))

    (define (forget retractor)
      (when (eq? retractor informant)
           ;(display `(,retractor retracting on ,this))(newline)
           (set! informant false)
           (set! v unset)
           (for ([x _observers]
                 #:unless (eq? x retractor))
             (send x reevaluate))))

    (define (getValue)
      v)

    (define (connect new-observer)
      (set! _observers (set-add _observers new-observer)))

    (define (disconnect c)
      (set! _observers (set-remove _observers c)))

    (define (neighbors)
      (getObservers))

    (define (getObservers)
      (set->list _observers))

    (public custom-write custom-display)
    (public
      (forget forgetValue!)
      (_set setValue!)
      requestSetValue!
      getObservers
      neighbors
      hasValue?
      connect
      disconnect
      informant?
      getName
      getValue)))

(define (connect connector observer [p null])
  (send connector
        connect observer)
  (send observer
        attach p connector)
  (send observer resolve))

; returns true if a is consistent with b
(define (consistent? a b) 
  (or (equal? a b)
      (and (is-a? a Value) (send a isConsistentWith? b))
      (and (is-a? b Value) (send b isConsistentWith? a))))

(define (exn:contradiction oldval newval connector)
  (list 'contradiction ': 'setting newval 'from oldval 'on connector))

(define (exn:contradiction? maybe-exn)
  (if (list? maybe-exn)
    (eq? 'contradiction (first maybe-exn))
    false))

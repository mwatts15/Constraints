#lang racket

(provide (all-defined-out))
(require "connector.rkt")
(require "traversable.rkt")

(define unset?
  (curry eq? 'unset))

(define is-set?
  (negate unset?))

(define Constraint
  (class* object% (writable<%> ConnectorObserver Traverseable)
    (super-new)
    (init (ports '()))
    (init-field [name 'GenericConstraint])
    (define _connectors (make-hash (map (lambda (l) (cons l 'unset))
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

    (define (getName)
      name)

    (define (reevaluate)
      (for ([(k c) _connectors])
        ;(display k)(newline)
        (send c forgetValue! this))
      (send this resolve))

    (define (disconnect port)
      (hash-set! _connectors port 'unset))

    (define (attach port connector)
      (hash-set! _connectors port connector))

    (define (getPort port-name)
      (hash-ref _connectors port-name))

    (define (connectorNames)
      (dict-keys _connectors))

    (define (getConnectors)
      (dict-values _connectors))

    (define (neighbors) (getConnectors))

    (public getPort 
            resolve reevaluate 
            connectorNames
            getConnectors
            getName
            neighbors
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
    (super-new (ports '(head tail list)) [name 'List])
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
                 (when (not (empty? ov))
                   (send t setValue! (rest ov) this)
                   (send f setValue! (first ov) this))]))))))
(define Array
  (class Constraint
    (super-new [ports '(index value array)] [name 'Array])
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

; holds a constant and matches the value of its sole connector.
; change the value though the methods setValue! and forgetValue!
(define Constant
  (class Constraint
    (super-new (ports '(out)) [name 'Constant])
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
  (when (is-set? (send c getPort port))
    (send (send c getPort port) disconnect c)
    (send c disconnect port)))

(define (networkEdges . verts)
  (define (name-list l)
    (map (lambda (x) (send x getName)) l))
  (define (helper _seen _verts _edges)
    (define (neighbors x) 
      (with-handlers ([(const #t) (lambda (_) (raise `(,x is not traversable on seen = ,_seen verts = ,_verts edges = ,_edges)))])
        (send x neighbors)))
    (define (edges x) (map (lambda (y) (list x y)) (neighbors x)))
    (define (seen? v) 
      (member v _seen))
    (if (empty? _verts)
      _edges
      ; take verts
      ; get their neighbors
      ; get the union of their neighbors
      (let* ([neighborVerts (remove-duplicates ((compose (curry apply append)
                                                         (curry map neighbors))
                                                _verts))]
             [newVerts (filter-not seen? neighborVerts)]
             [neighborEdges (remove-duplicates ((compose (curry apply append)
                                                         (curry map edges))
                                                _verts))]
             [newEdges (remove-duplicates
                         (append neighborEdges _edges)
                         (lambda (x y) (or (equal? x y)
                                           (equal? (reverse x) y))))]
             [newSeen (remove-duplicates (append _verts _seen))])
        (helper newSeen newVerts newEdges))))
        
  (remove-duplicates (helper '() verts '())))

(define (graphvizOut . verts)
  (define names (make-hash))
  (define (getName c)
    ((compose (lambda (n) (regexp-replace "-" n "_"))
              (curry format "~a"))
     (dict-ref! names c (gensym (send c getName)))))

  (printf "graph g {~n")
  (printf "overlap=false~n")
  (for ([e (apply networkEdges verts)])
    (when (is-a? (car e) ConnectorObserver)
      (printf "~a [shape=box]~n" (getName (car e))))
    (when (is-a? (cdr e) ConnectorObserver)
      (printf "~a [shape=box]~n" (getName (cdr e))))
    (printf "~a--~a~n" (getName (first e)) (getName (second e))))
  (printf "}~n"))

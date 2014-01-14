#lang racket

(require "connector.rkt"
         "traversable.rkt"
         "unset.rkt")

(provide (all-defined-out)
         (all-from-out "unset.rkt"))

(define Constraint
  (class* object% (writable<%> ConnectorObserver Traverseable)
    (super-new)
    (init [ports '()])
    (init-field [name 'GenericConstraint])
    (define _connectors (make-hash (map (lambda (l) (cons l unset))
                                       ports)))
    (define (custom-write out)
      (display name out))

    (define (custom-display out)
      (send this custom-write out))
    ; Try to resolve
    ; return If the constraints were satisfied, true
    ;        otherwise false
    (define (resolve)
      (define (allPortsAreConnected)
        (andmap is-set? (dict-values _connectors)))
      (when (allPortsAreConnected)
        (inner (void) resolve)))

    (define (getName)
      name)

    (define (reevaluate)
      (for ([(k c) _connectors])
        ;(display k)(newline)
        (send c forgetValue! this))
      (send this resolve))

    (define (disconnect port)
      (hash-update! _connectors port (const unset)))

    (define (attach port connector)
      ;(displayln `(attaching ,connector to ,this at ,port))
      (hash-update! _connectors port (const connector)))

    (define (getPort port-name)
      (hash-ref _connectors port-name))
    (define (getConnector port)
      (getPort port))

    (define (setPortValue! p v s)
      (send (hash-ref _connectors p) setValue! v s))

    (define (portNames)
      (connectorNames))

    (define (connectorNames)
      (dict-keys _connectors))

    (define (getConnectors)
      (dict-values _connectors))

    (define (neighbors) (getConnectors))

    (pubment resolve)
    (public getPort 
            getConnector
            setPortValue!
            reevaluate 
            attach
            connectorNames
            portNames
            getConnectors
            getName
            neighbors
            disconnect)
    ; for writeable<%>
    (public custom-write custom-display)))

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

; because f->c needs it...
(define Constant
  (class Constraint
    (super-new [ports '(toSet)] [name 'Constant])
    (init value connector)
    (inherit getPort)
    (define _v value)
    (define (getValue) _v)

    (define/augment (resolve)
      (let ([p (getPort 'toSet)])
        (send p setValue! _v this)))
    (connect connector this 'toSet)
    (public getValue)))

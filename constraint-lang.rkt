#lang racket

(require "constraint-base.rkt"
         "console-rep.rkt"
         racket/trace
         "connector.rkt")
(provide f->c)
; the basic format is a list of constraints, which can be thought of as
; a conjunction
(define (f->c formulas userOps #:name [constraintName 'UserConstraint] )

  (define (checkBindings constraint bindings)
    (let ([names (map first bindings)]
          [connectors (send constraint connectorNames)])
      (for ([v names])
        (unless (member v connectors)
          (error (format "not a port for ~a: ~a" constraint v))))))

  (class Constraint
    (define vstore (make-hash))

    (define (storeConnection connectorName constraint port)
      (dict-update! vstore connectorName (curry cons (cons constraint port))
                    '()))
    ; we store the internal constraint and port to attach to
    ; for each variable, p 
    ;
    ; at attach, we need 
    ; * port name(s) on the internal constraint(s) (different from p). stored 
    ; * internal constraint(s). stored 
    ; * a connector. provided
    (define (attach p c)
      (for ([binding (dict-ref vstore p)])
        (match-let ([`(,constraint . ,portName) binding])
          (connect c constraint portName))))

    (for ([f formulas])
      (if (eq? (first f) '=)
        (let* ([name (second f)]
               [form (rest (rest f))])
          (set! userOps (hash-set userOps name (f->c form userOps #:name name))))
        (match-let* ([(cons cName bindings) f] 
                     [c (new (dict-ref userOps cName))])
          (checkBindings c bindings)
          (for ([b bindings])
            (let* ([varName (second b)]
                   [portName (first b)])
              (if (symbol? varName) ; then it's a constant
                (storeConnection varName c portName)
                (let ([connector (new Connector)])
                  (connect connector c portName)
                  (new Constant [connector connector][value varName]))))))))
    (super-new [ports (dict-keys vstore)] [name constraintName])
    (for ([p (send this portNames)])
      (let ([c (new Connector [name p])])
        (new ConsoleRep [c c])
        (attach p c)))
    (override attach)))

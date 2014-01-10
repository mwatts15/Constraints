#lang racket

(require "constraint-base.rkt"
         racket/trace
         "connector.rkt")
(provide f->c)
; the basic format is a list of constraints, which can be thought of as
; a conjunction
(define (f->c formulas userOps #:name [constraintName 'UserConstraint] )

  (define vstore (make-hash))

  (define (getVar v)
    (dict-ref! vstore v (lambda ()
                          (new Connector [name v]))))
  (define (checkBindings constraint bindings)
    (let ([names (map first bindings)]
          [connectors (send constraint connectorNames)])
      (for ([v names])
        (unless (member v connectors)
          (error (format "not a port for ~a: ~a" constraint v))))))

  (define (external vstore)
    (class Constraint
      (super-new [ports (dict-keys vstore)] [name constraintName])
      (define/override (resolve) (void))
      (for ([(k v) vstore])
        (send this attach k v))))

  (if (eq? (caar formulas) '=)
    (let* ([f (first formulas)]
           [r (rest formulas)]
           [name (cadr f)]
           [form (cddr f)])
      (f->c r (hash-set userOps 
                        name (f->c form userOps #:name name))
            #:name constraintName))
    (for ([f formulas])
      (match-let* ([(cons cName bindings) f] 
                   [c (new (dict-ref userOps cName))])
        (checkBindings c bindings)
        (for ([b bindings])
          (let ([connector (getVar (second b))]
                [port (first b)])
            (connect connector c port))))))
  (external vstore))

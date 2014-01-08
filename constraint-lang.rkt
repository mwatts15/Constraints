#lang racket

(require "constraint-base.rkt"
         "math-base.rkt"
         "connector.rkt")
(provide f->c)
; the basic format is a list of constraints, which can be thought of as
; a conjunction
(define (f->c formulas userOps #:name [constraintName 'UserConstraint] )

  (define (external vars)
    (class Constraint
      (super-new [ports (dict-keys vars)] [name constraintName])
      (define/override (resolve) (void))
      (for ([(k v) vars])
        (send this attach k v))))

  (define vstore (make-hash))

  (define (getVar v)
    (dict-ref! vstore v (lambda ()
                          (new Connector [name v]))))
  (define _ops userOps)

  (for ([f formulas])
    (let* ([c (new (dict-ref _ops (first f)))]
           [connectors (send c connectorNames)])

      (for ([v (map first (rest f))])
        (unless (member v connectors)
          (error (format "not a port for ~a: ~a" c v))))

      (for ([arg (rest f)])
        (let ([connector (getVar (second arg))]
              [port (first arg)])
          (connect connector c port)))))
  (external vstore))

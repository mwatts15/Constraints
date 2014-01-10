#lang racket

(require (prefix-in O: "object.rkt"))
(provide (all-defined-out))

; stores Objects and data that should be associated with them
(define ObjectStore
  (class object% 
    (super-new)
    (define _selected #f)
    (define _types (make-hash `(("Rectangle" . ,O:Rectangle)
                                ("List" . ,O:List))))
    (define _objects (make-hash))
    (init [selectionCallback void])

    ; The representation doesn't have ownership of the variables.
    ; The variable store keeps them and makes the name
    (define (setSelected objectID)
      (set! _selected objectID)
      (onSelectionChange this))

    (define onSelectionChange selectionCallback)

    (define (sendSelected the-method . &args)
      (send/apply (dict-ref _objects _selected) -> the-method &args))

    (define (selected? objectName)
      (eq? objectName _selected))

    ; The created object isn't guaranteed to have the given name.
    (define (newObject type name dc)
      (define (getName base i)
        (let ([n (format "~a~a" name i)])
          (cond [(not (dict-has-key? _objects base)) name]
                [(not (dict-has-key? _objects n)) n]
                [#t (getName base (add1 i))])))
      (let ([n (getName name 0)]
            [typeClass (dict-ref _types type)])
        (dict-set! _objects n (new typeClass [name n] 
                                   [drawingContext dc]))
        (setSelected n)))

    (define (foreach f)
      (for ([(name o) _objects])
        (f name o)))

    (public foreach newObject setSelected sendSelected)))


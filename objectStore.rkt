#lang racket

(require (prefix-in O: "object.rkt"))
(provide (all-defined-out))

(define ObjectStore
  (class object% 
    (super-new)
    (define _selected #f)
    (define _types (make-hash `(("Rectangle" . ,O:Rectangle)
                                ("List" . ,O:List))))
    (define _objects (make-hash))
    (init drawingContext)
    (init [selectionCallback void])
    (define _dc drawingContext)

    ; The representation doesn't have ownership of the variables.
    ; The variable store keeps them and makes the name
    (define (setSelected objectID)
      (set! _selected objectID)
      (onSelectionChange this))

    (define (getObjectByLocation location)
      (let ([candidateSelections 
              (filter
                (lambda (o) 
                  (send (dict-ref _objects o) -> 'inside? location))
                (dict-keys _objects))])
        (if (empty? candidateSelections)
          #f
          (first candidateSelections))))

    (define (setSelectedByLocation location)
      (let ([candidateSelections 
              (filter
                (lambda (o) 
                  (send (dict-ref _objects o) -> 'inside? location))
                (dict-keys _objects))])
        (if (empty? candidateSelections)
          #f
          (begin 
            (setSelected (first candidateSelections))
            #t))))

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

    (public foreach newObject getObjectByLocation setSelectedByLocation setSelected sendSelected)))


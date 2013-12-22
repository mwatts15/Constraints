#lang racket
(require racket/gui/base)
(require racket/draw)
(require "value.rkt")
(require "connector.rkt")
(require (prefix-in O: "object.rkt"))
(require trace)

(define no-pen (new pen% [style 'transparent]))
(define no-brush (new brush% [style 'transparent]))
(define blue-brush (new brush% [color "blue"]))
(define yellow-brush (new brush% [color "yellow"]))
(define red-pen (new pen% [color "red"] [width 2]))
(define black-pen (new pen% [color "black"] [width 2]))

; a store for drawable objects. resonds to draw and holds a canvas
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

    (define (getSelected)
      (hash-ref _objects _selected))

    (define onSelectionChange selectionCallback)

    (define (sendSelected the-method . &args)
      (send/apply (dict-ref _objects _selected) -> the-method &args))

    (define (newObject type name dc)
      (define (getName base i)
        (let ([n (format "~a~a" name i)])
          (cond [(not (dict-has-key? _objects base)) name]
                [(not (dict-has-key? _objects n)) n]
                [#t (getName base (add1 i))])))
      (let ([n (getName name 0)]
            [typeClass (dict-ref _types type)])
        (dict-set! _objects n (new typeClass [drawingContext dc]))
        (setSelected n)))

    (define (draw)
      (send _dc clear)
      (for ([(name o) _objects])
        (let* ([p (send o -> 'getPos)]
               [x (send p get-x)]
               [y (send p get-y)]
               [oDC (send o get-dc)])
          (send _dc draw-bitmap (send oDC get-bitmap) x y)
          (send _dc set-brush yellow-brush)
          (when (eq? name _selected) 
            (send _dc draw-ellipse x y 5 5)))))

    (public newObject draw setSelectedByLocation setSelected sendSelected)))

(define resizable-bitmap-dc%
  (class bitmap-dc%
    (super-new [bitmap (make-screen-bitmap 1 1)])
    (define (requestSize w h)
      (let ([w (inexact->exact w)]
            [h (inexact->exact h)])
        (let-values ([(cw ch) (send this get-size)])
          (when (not (and (<= w cw) (<= h ch)))
            (let* ([b (make-screen-bitmap w h)]
                   [bdc (make-object bitmap-dc% b)])
              (send bdc draw-bitmap (send this get-bitmap) 0 0)
              (send this set-bitmap b))))))
    (public requestSize)))

(define draw-canvas%
  (class canvas%
    (super-new)

    ; selection and dragging
    (define objectShouldDrag? #f) ; should the selected object be dragged?
    (define dragOffset (make-point 0 0)) ; offset of mouse from object location when dragging
    (define selectionStartCorner (make-point 0 0))
    (define selectionEndCorner (make-point 0 0))
    (define shouldDrawSelectionBox? #f)
    (define dc (send this get-dc))
    (define (rectFromPoints p1 p2)
      (let ([x1 (send p1 get-x)]
            [x2 (send p2 get-x)]
            [y1 (send p1 get-y)]
            [y2 (send p2 get-y)])
        (values (min x1 x2) (min y1 y2)
                (max x1 x2) (max y1 y2))))

    (define/override (on-subwindow-event receiver event)
      (let ([cursorPoint (make-point (send event get-x) (send event get-y))])
        ; selection
        (cond [(send event button-down?)
               (if (send objects setSelectedByLocation cursorPoint)
                 (begin (set! objectShouldDrag? #t)
                        (set! dragOffset (sub/pp (send objects sendSelected 'getPos) cursorPoint)))
                 (begin (set! selectionStartCorner cursorPoint)
                        (set! shouldDrawSelectionBox? #t)))
               (send objects draw)]
              [(send event button-up?)
               (set! objectShouldDrag? #f)
               (set! shouldDrawSelectionBox? #f)
               (send objects draw)]
              [(send event dragging?)
               (cond [objectShouldDrag?
                       (send objects sendSelected 'setPos (add/pv cursorPoint dragOffset))
                       (send objects draw)]
                     [shouldDrawSelectionBox?
                       (send objects draw)
                       (set! selectionEndCorner cursorPoint)
                       (let-values ([(x1 y1 x2 y2)
                                     (rectFromPoints selectionStartCorner selectionEndCorner)])
                         (send dc set-pen "grey50" 2 'short-dash)
                         (send dc draw-rectangle x1 y1 (- x2 x1) (- y2 y1)))])])))))

(define my-frame% 
  (class frame%
    (super-new)))

(define w (new my-frame% [label "Silly window"]))

(define the-canvas (new draw-canvas% [parent w]))
(define objects (new ObjectStore [drawingContext (send the-canvas get-dc)]))

(define add-rectangle-button
  (new button% 
       [label "Add Rectangle"]
       [callback (lambda (b e)
                   (send objects newObject "Rectangle" (send new-object-name get-value)
                         (new resizable-bitmap-dc%))
                   (send objects draw))]
       [parent w]))
(define add-list-button
  (new button% 
       [label "Add List"]
       [callback (lambda (b e)
                   (send objects newObject "List" (send new-object-name get-value)
                         (new resizable-bitmap-dc%))
                   (send objects draw))]
       [parent w]))
(define cons-num-list-button
  (new button% 
       [label "Cons onto List"]
       [callback (lambda (b e)
                   (send objects sendSelected 'cons 5)
                   (send objects draw))]
       [parent w]))

(define new-object-name
  (new text-field%
       [label "New object name"]
       [init-value "Object"]
       [parent w]))

(send w show "hi")

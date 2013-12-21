#lang racket
(require racket/gui/base)
(require racket/draw)
(require "variables.rkt")
(require "value.rkt")
(require (prefix-in O: "object.rkt"))
(require trace)

(define no-pen (new pen% [style 'transparent]))
(define no-brush (new brush% [style 'transparent]))
(define blue-brush (new brush% [color "blue"]))
(define yellow-brush (new brush% [color "yellow"]))
(define red-pen (new pen% [color "red"] [width 2]))
(define black-pen (new pen% [color "black"] [width 2]))

; add a point to a vector
(define (add/pv p v)
  (make-object point% (+ (send p get-x) (send v get-x))
               (+ (send p get-y) (send v get-y))))

(define (sub/pp p1 p2)
  (make-object point% (- (send p1 get-x) (send p2 get-x))
               (- (send p1 get-y) (send p2 get-y))))


; a store for drawable objects. resonds to draw and holds a canvas
(define ObjectStore
  (class object% 
    (super-new)
    (define _selected #f)
    (define _types (make-hash `(("Rectangle" . ,O:Rectangle))))
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
      (dict-set! _objects (getName name 0) (new (dict-ref _types type) [drawingContext dc])))

    (define (draw)
      (send _dc clear)
      (for ([(name o) _objects])
        (display 'drawing-here)(newline)
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
      (let-values ([(cw ch) (send this get-size)])
        (when (not (and (= w cw) (= h ch)))
          (send this set-bitmap (make-screen-bitmap w h)))))
    (public requestSize)))

(define draw-canvas%
  (class canvas%
    (super-new)

    ; selection and dragging
    (define objectShouldDrag? #f) ; should the selected object be dragged?
    (define dragOffset (make-point 0 0)) ; offset of mouse from object location when dragging

    (define/override (on-subwindow-event receiver event)
      (let ([cursorPoint (make-point (send event get-x) (send event get-y))])
        ; selection
        (cond [(and (send event button-down?) (send objects setSelectedByLocation cursorPoint))
               (set! objectShouldDrag? #t)
               (set! dragOffset (sub/pp (send objects sendSelected 'getPos) cursorPoint))
               (send objects draw)]
              [(send event button-up?)
               (set! objectShouldDrag? #f)]
              [(and (send event dragging?) objectShouldDrag?)
               (send objects sendSelected 'setPos (add/pv cursorPoint dragOffset))
               (send objects draw)])))))

(define my-frame% 
  (class frame%
    (super-new)))

(define w (new my-frame% [label "Silly window"]))

;(define ListBoxRep
  ;(class list-box%
    ;(init parent)
    ;(super-new [label "Variables"]
       ;[choices '("no constraints")]
       ;[parent w]
       ;[style '(multiple)]
       ;[columns '("name" "value")])
    ;(define _v 'unset)
    ;; a list of the string versions of the options
    ;; only changes on a setvar
    ;(define _options (hash))

    ;(define (setVar v)
      ;(send this clear)
      ;(set! _v v)
      ;(set! _options
        ;(for/vector ([m (send _v memberNames)])
          ;(let ([mstr (format "~a" m)]
                ;[mv (send _v getMember m)])
            ;(connect mv this)
            ;(send this append mstr)
            ;(list mstr mv)))))

    ;(define (attach p c) #f)
    ;(define (reevaluate) (resolve))
    ;(define (resolve)
      ;(for ([(data idx) (in-indexed _options)])
        ;(let ([varname (first data)]
              ;[value (format "~a" (send (second data) getValue))])
          ;(send this set-string idx varname 0)
          ;(send this set-string idx value 1))))
    ;(public attach resolve reevaluate setVar)))

;(define selectedAttributesView
  ;(new ListBoxRep [parent w]))

;(define (updateSelectedAttributes rep)
  ;(let ([v (send rep sendSelected 'getVar)])
    ;(send selectedAttributesView setVar v)))

(define the-canvas (new draw-canvas% [parent w]))
(define objects (new ObjectStore [drawingContext (send the-canvas get-dc)]))

(define add-rectangle-button
  (new button% 
       [label "Add square"]
       [callback (lambda (b e)
                   (send objects newObject "Rectangle" (send new-object-name get-value)
                         (new resizable-bitmap-dc%))
                   (send objects draw))]
       [parent w]))

(define new-object-name
  (new text-field%
       [label "New object name"]
       [init-value "Object"]
       [parent w]))

(send w show "hi")
;(define mahrect (new rect% [w 20] [h 12]))
;(display mahrect)


#lang racket
(require racket/gui/base)
(require racket/draw)
(require "constraint.rkt")
(require "variables.rkt")

(define no-pen (new pen% [style 'transparent]))
(define no-brush (new brush% [style 'transparent]))
(define blue-brush (new brush% [color "blue"]))
(define yellow-brush (new brush% [color "yellow"]))
(define red-pen (new pen% [color "red"] [width 2]))

(define Drawable
  (interface () draw))

; A rep can access members. Consequently, it requires that
; the variable connected has the members you need.
;
; it would be useful to have a correspondence of any variable 
; to the width, x, and y
; It would be possible to provide this functionality, but it
; would be easier to define a grouping variable that defines
; the appropriate members given a set of variables...
(define SquareRep
  (class object%
    (super-new)

    (init drawingContext)
    (init connector)

    (define _x (get-field x connector))
    (define _y (get-field y connector))
    (define _width (get-field width connector))
    (define _currPos (make-point 0 0))
    (define _currWidth 20)
    (define _name (send connector getName)) 
    (define _dc drawingContext)

    (define (setPos p)
      (send _x setValue! (send p get-x) this)
      (send _y setValue! (send p get-y) this))

    (define (setWidth w)
      (send _width setValue! w this))

    (define (resolve)
      (and (send _x hasValue?)
           (send _currPos set-x (send _x getValue)))
      (and (send _y hasValue?)
           (send _currPos set-y (send _y getValue)))
      (and (send _width hasValue?)
           (set! _currWidth (send _width getValue)))
      (send _dc set-brush blue-brush)
      (send _dc draw-rectangle (send _currPos get-x)
            (send _currPos get-y) _currWidth _currWidth))

    (define (reevaluate) (resolve))

    (define (disconnect port)
      (error "nope"))

    (define (attach port c)
      void)

    (define (inside? p)
      (let ([px (send p get-x)]
            [py (send p get-y)]
            [x (send _currPos get-x)]
            [y (send _currPos get-y)]
            [w _currWidth])
        (and (> px x) (> py y) (< px (+ x w)) (< py (+ y w)))))

    (define (getName)
      _name)

    (connect _x this)
    (connect _y this)
    (connect _width this)

    (public setPos setWidth resolve reevaluate 
            inside? getName
            disconnect attach)))

(define (make-point x y)
  (make-object point% x y))

; vars have can contain many variables, each with a name
; a var is analogous to a rule in prolog and can contain
; a number of variables.
; box(b), valBox(b)
; hasColor(b,c) <- equals(c, b.c)
; 

; a store for variables
(define State
  (class object%
    (super-new)
    (define vars (make-hash))

    (define (newVar class) 
      (if (subclass? class Variable)
        (let ([o (new class)])
          (hash-set! vars (send o getName) o)
          (send o getName))
        (error "cannot make a variable of non-variable type" class)))

    (define (getVar name)
      (hash-ref vars name))

    (public newVar 
            getVar)))

; a store for drawable objects. resonds to draw and holds a canvas
(define Representation
  (class object% 
    (super-new)
    (define _selected 'unset)
    (define _objects (make-hash))
    (init drawingContext)
    (init [selectionCallback void])
    (define _dc drawingContext)


    (define (setSelected objectID)
      (set! _selected objectID)
      (onSelectionChange))

    (define (setSelectedByLocation location)
      (let ([candidateSelections 
              (filter
                (lambda (o) 
                  (send (dict-ref _objects o) inside? location))
                (dict-keys _objects))])
        (if (empty? candidateSelections)
          #f
          (setSelected (first candidateSelections)))))

    (define (getSelected)
      (hash-ref _objects _selected))

    (define onSelectionChange selectionCallback)

    (define (sendSelected the-method . &args)
      (apply dynamic-send (dict-ref _objects _selected) the-method &args))

    (define (newRepresentation repKind var)
      (let ([name (send var getName)])
        (dict-set! _objects name (new repKind [drawingContext _dc] [connector var]))
        (setSelected name)
        name))

    (define (draw)
      (send _dc clear)
      (for ([o (in-dict-values _objects)])
        (send o resolve)))
    (public draw newRepresentation setSelectedByLocation setSelected getSelected sendSelected)))



(define draw-canvas%
  (class canvas%
    (super-new )

    (define dc (send this get-dc))

    (define state (new State))

    (define rep (new Representation [drawingContext dc]))

    (define/public (add-square)
      (send rep newRepresentation SquareRep
            (send state getVar (send state newVar SquareV)))
      (redraw))

    (define/override (on-subwindow-event receiver event)
      (let-values ([(x y) (values (send event get-x) (send event get-y))])
        (cond [(send event button-down?)
               (send rep setSelectedByLocation (make-point x y))
               (display `(selected is ,(send rep sendSelected 'getName)))(newline)
               (redraw)]
              [(send event dragging?)
               (send rep sendSelected 'setPos (make-point x y))
               (redraw)])))

    (define (redraw)
      (send rep draw))))

(define my-frame% 
  (class frame%
    (super-new)))

(define w (new my-frame% [label "Silly window"]))

;(define selectedAttributesView
  ;(new list-box% [label "Variables"] [choices '("no constraints")] [parent w]))

;(define (updateSelectedAttributes)
  ;(send selectedAttributesView set (list "updated selected attributes")))

(define the-canvas (new draw-canvas% [parent w]))

(define add-square-button
  (new button% 
       [label "Add square"]
       [callback (lambda (b e)
                   (send the-canvas add-square))]
       [parent w]))

(define add-color-button
  (new button% 
       [label "Change color"]
       [callback (lambda (b e)
                   (printf "pick a color~n"))]
       [parent w]))

(send w show "hi")
;(define mahrect (new rect% [w 20] [h 12]))
;(display mahrect)


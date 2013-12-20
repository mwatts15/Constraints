#lang racket
(require racket/gui/base)
(require racket/draw)
(require "constraint.rkt")
(require "variables.rkt")
(require trace)

(define no-pen (new pen% [style 'transparent]))
(define no-brush (new brush% [style 'transparent]))
(define blue-brush (new brush% [color "blue"]))
(define yellow-brush (new brush% [color "yellow"]))
(define red-pen (new pen% [color "red"] [width 2]))
(define black-pen (new pen% [color "black"] [width 2]))

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

    (define _c connector)
    (define _x (getMember connector x))
    (define _y (getMember connector y))
    (define _width (getMember connector w))
    (define (getVar)
      _c)
    (define (getMembers)
      (values (list "x" "y" "width")
              (map (lambda (x) (format "~a" x))
                   (list (send _x getValue)
                    (send _y getValue)
                    (send _width getValue)))))
    (define _currPos (make-point 0 0))
    (define _currWidth 50)
    (define _name (send connector getName)) 
    (define _dc drawingContext)
    (define _outline black-pen)

    (define (setPos p)
      (send _x setValue! (send p get-x) this)
      (send _y setValue! (send p get-y) this))

    (define (getPos)
      _currPos)

    (define (setWidth w)
      (send _width setValue! w this))

    (define (resolve)
      (and (send _x hasValue?)
           (send _currPos set-x (send _x getValue)))
      (and (send _y hasValue?)
           (send _currPos set-y (send _y getValue)))
      (and (send _width hasValue?)
           (set! _currWidth (send _width getValue)))
      (send _dc set-pen _outline)
      (send _dc set-brush blue-brush)
      (send _dc draw-rectangle (send _currPos get-x)
            (send _currPos get-y) _currWidth _currWidth))

    (define (reevaluate) (resolve))

    (define (setOutilne color)
      (cond [(eq? color "red")
             (set! _outline red-pen)]
            [(eq? color "black")
             (set! _outline black-pen)]))

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
            getVar
            getPos
            getMembers
            setOutilne
            inside? getName
            disconnect attach)))

(define (make-point x y)
  (make-object point% x y))

; add a point to a vector
(define (add/pv p v)
  (make-object point% (+ (send p get-x) (send v get-x))
               (+ (send p get-y) (send v get-y))))

(define (sub/pp p1 p2)
  (make-object point% (- (send p1 get-x) (send p2 get-x))
               (- (send p1 get-y) (send p2 get-y))))

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
      (and (not (unset? _selected))
           (send (dict-ref _objects _selected) setOutilne "black"))
      (send (dict-ref _objects objectID) setOutilne "red")
      (set! _selected objectID)
      (onSelectionChange this))

    (define (setSelectedByLocation location)
      (let ([candidateSelections 
              (filter
                (lambda (o) 
                  (send (dict-ref _objects o) inside? location))
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
      (apply dynamic-send (dict-ref _objects _selected) the-method &args))

    (define (newRepresentation repKind var)
      (let ([name (send var getName)])
        (dict-set! _objects name (new repKind [drawingContext _dc] [connector var]))
        (setSelected name)
        name))

    (define (draw)
      (send _dc clear)
      (for ([(name o) _objects])
        (send o resolve)))
    (public draw newRepresentation setSelectedByLocation setSelected sendSelected)))

(define draw-canvas%
  (class canvas%
    (super-new)

    (define dc (send this get-dc))

    (define state (new State))

    (define rep (new Representation [drawingContext dc]
                     [selectionCallback updateSelectedAttributes]))

    ; selection and dragging
    (define objectShouldDrag? #f) ; should the selected object be dragged?
    (define dragOffset (make-point 0 0)) ; offset of mouse from object location when dragging

    (define/public (add-square)
      (send rep newRepresentation SquareRep
            (send state getVar (send state newVar SquareV)))
      (redraw))

    (define/override (on-subwindow-event receiver event)
      (let ([cursorPoint (make-point (send event get-x) (send event get-y))])
        ; selection
        (cond [(and (send event button-down?)
                    (send rep setSelectedByLocation cursorPoint))
               (set! objectShouldDrag? #t)
               (set! dragOffset (sub/pp (send rep sendSelected 'getPos) cursorPoint))
               (redraw)]
              [(send event button-up?)
               (set! objectShouldDrag? #f)]
              [(and (send event dragging?) objectShouldDrag?)
               (send rep sendSelected 'setPos (add/pv cursorPoint dragOffset))
               (redraw)])))

    (define (redraw)
      (send rep draw))))

(define my-frame% 
  (class frame%
    (super-new)))

(define w (new my-frame% [label "Silly window"]))

(define ListBoxRep
  (class list-box%
    (init parent)
    (super-new [label "Variables"]
       [choices '("no constraints")]
       [parent w]
       [style '(multiple)]
       [columns '("name" "value")])
    (define _v 'unset)
    ; a list of the string versions of the options
    ; only changes on a setvar
    (define _options (hash))

    (define (setVar v)
      (send this clear)
      (set! _v v)
      (set! _options
        (for/vector ([m (send _v memberNames)])
          (let ([mstr (format "~a" m)]
                [mv (send _v getMember m)])
            (connect mv this)
            (send this append mstr)
            (list mstr mv)))))

    (define (attach p c) #f)
    (define (reevaluate) (resolve))
    (define (resolve)
      (for ([(data idx) (in-indexed _options)])
        (let ([varname (first data)]
              [value (format "~a" (send (second data) getValue))])
          (send this set-string idx varname 0)
          (send this set-string idx value 1))))
    (public attach resolve reevaluate setVar)))

(define selectedAttributesView
  (new ListBoxRep [parent w]))

(define (updateSelectedAttributes rep)
  (let ([v (send rep sendSelected 'getVar)])
    (send selectedAttributesView setVar v)))

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


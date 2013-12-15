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
    (field [dc null])
    (init-field connector)
    (define _x (get-field x connector))
    (define _y (get-field y connector))
    (define _width (get-field width connector))

    (define _currPos (make-point 0 0))
    (define (setPos)
      (and (send _x hasValue?)
           (send _currPos set-x (send _x getValue)))
      (and (send _y hasValue?)
           (send _currPos set-y (send _y getValue))))

    (define _currWidth 20)
    (define (setWidth)
      (and (send _width hasValue?)
           (set! _currWidth (send _width getValue))))

    (connect _x this)
    (connect _y this)
    (connect _width this)
    (setPos)
    (setWidth)
    (define (resolve)
      (setPos)
      (setWidth)
      (printf "~a,~a~n" (send _currPos get-x)(send _currPos get-y))
      (send dc set-brush blue-brush)
      (send dc draw-rectangle (send _currPos get-x)
            (send _currPos get-y) _currWidth _currWidth))

    (define (reevaluate) (resolve))

    (define (disconnect port)
      (error "nope"))

    (define (attach port c)
      void)

    (public resolve reevaluate 
            disconnect attach)))

(define (make-point x y)
  (make-object point% x y))

(define rect%
  (class object%
    (super-new)
    [init-field w h]))

; vars have can contain many variables, each with a name
; a var is analogous to a rule in prolog and can contain
; a number of variables.
; box(b), valBox(b)
; hasColor(b,c) <- equals(c, b.c)
; 
; we are endowing variables with additional functionality that allows
; for them to aggregate other variables
;
; most of the things we do to a var are things we do to a variable
; however, a var can also be drawn...
;(define/override (draw dc)
      ;(let ([x (send pos get-x)]
            ;[y (send pos get-y)]
            ;[w (get-field w the-rect)]
            ;[h (get-field h the-rect)])
        ;(printf "drawing at ~a ~a~n" x y)
        ;(send dc set-brush blue-brush)
        ;(send dc draw-rectangle x y w h)))

; a store for variables
(define State
  (class object%
    (super-new)
    (define vars (list))

    (define (setSelected objectID)
      (onSelectionChange))

    (define (onSelectionChange)
      (updateSelectedAttributes))

    (define (newVar class) 
      (let ([o (new class)])
        (set! vars (cons o vars))
        (setSelected 0)))

    (define (send-selected the-method . &args)
      (apply dynamic-send (first vars) the-method &args))

    (public newVar 
            (send-selected ->selected))))

; a store for drawable objects. resonds to draw and holds a canvas
(define ObjectHolder
  (class object% 
    (super-new)
    (define objects '())
    (define (drawObjects)
      (for ([o objects])
        (send o draw)))))

(define state (new State))

(define (add-square)
  (send state newVar SquareV)
  (send state ->selected 'getConstraints))

(define the-square-var
  (new ObjectV))

(define the-square-rep
  (new SquareRep [connector the-square-var]))

(define the-square-var2
  (new ObjectV))

(define the-square-rep2
  (new SquareRep [connector the-square-var2]))

(define lo (new LeftOf))
(define t (new Variable))
(connect the-square-var lo 'o1)
(connect the-square-var2 lo 'o2)
(connect t lo 'out)

(define draw-canvas%
  (class canvas%
    (super-new )
    (define dc (send this get-dc))
    (set-field! dc the-square-rep dc)
    (set-field! dc the-square-rep2 dc)
    (define/override (on-subwindow-event receiver event)
      (let-values ([(x y) (values (send event get-x) (send event get-y))])
        (cond [(or (send event button-down?)
                   (send event dragging?))
               (send the-square-var setPos (cons x y))
               (redraw)])))
    (define (draw-state)
      (send dc clear)
      (send the-square-rep reevaluate)
      (send the-square-rep2 reevaluate))

    (define (redraw)
        (draw-state))))

(define my-frame% 
  (class frame%
    (super-new)))

(define w (new my-frame% [label "Silly window"]))

(define selectedAttributesView
  (new list-box% [label "Constraints"] [choices '("no constraints")] [parent w]))

(define (updateSelectedAttributes)
  (send selectedAttributesView set (send state ->selected 'getMembers)))

(define the-canvas (new draw-canvas% [parent w]))

(define add-square-button
  (new button% 
       [label "Add square"]
       [callback (lambda (b e)
                   (add-square))]
       [parent w]))

(define add-color-button
  (new button% 
       [label "Change color"]
       [callback (lambda (b e)
                   (printf "pick a color~n"))]
       [parent w]))

(send w show "hi")
(send t setValue! #t)
(send the-square-var setWidth 25)
;(define mahrect (new rect% [w 20] [h 12]))
;(display mahrect)


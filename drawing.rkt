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

(define SquareRep
  (class object%
    (super-new)
    (init-field connector)
    (connect (get-field x connector) this)
    (connect (get-field y connector) this)
    (connect (get-field width connector) this)
    (define (resolve)
      (printf "~a (~a,~a) w=~a ~n"
              (get-field name connector)
              (send (get-field x connector) getValue)
              (send (get-field y connector) getValue)
              (send (get-field width connector) getValue)))

    (define (reevaluate) (resolve))

    (define (disconnect port)
      (error "nope"))

    (define (attach port c)
      void)

    (public resolve reevaluate 
            disconnect attach)))

(define Representation
  (class object%
    (super-new)
    ; bounds
    [init (x 0) (y 0) (w 1) (h 1)]
    (field [pos (new point% (x x) (y y))])
    (define (resolve)
      (let ([x (car new-pos)] [y (cdr new-pos)])
        (set! pos (make-point x y))))
    
    (define (set-pos new-pos)
      (let ([x (car new-pos)] [y (cdr new-pos)])
        (printf "setting pos (~a,~a)~n" x y)
        (set! pos (make-point x y))))

    (define bounds (new rect% (w w) (h h)))
    (define (draw dc)
      (error "abstract"))
    (public draw set-pos)))

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

    (define (send-selected the-method)
      (dynamic-send (first vars) the-method))

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

; objects can be selected from the canvas
; some things might happen when the objects get
; selected, but I'm not sure what
(define draw-canvas%
  (class canvas%
    (super-new [paint-callback (lambda (c d) (send state draw d))])
    (define dc (send this get-dc))
    (define/override (on-subwindow-event receiver event)
      (let-values ([(x y) (values (send event get-x) (send event get-y))])
        (cond [(or (send event button-down?)
                   (send event dragging?))
               (send state setSelectedPos (cons x y))
               (redraw)])))
    (define (draw-state)
      (send state draw dc))

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
;(define mahrect (new rect% [w 20] [h 12]))
;(display mahrect)


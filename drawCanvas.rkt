#lang racket
(require racket/gui/base)
(require racket/draw)
(require (only-in "value.rkt" make-point))
(require "objectStore.rkt")

(provide draw-canvas%)
(define yellow-brush (new brush% [color "yellow"]))
(define black-pen (new pen% [color "black"] [width 2]))
(define draw-canvas%
  (class canvas%
    (super-new)
    (init objects)
    (define _objects objects)
    ; selection and dragging
    (define _dc (send this get-dc))

    (define (rectFromPoints p1 p2)
      (let ([x1 (send p1 get-x)]
            [x2 (send p2 get-x)]
            [y1 (send p1 get-y)]
            [y2 (send p2 get-y)])
        (values (min x1 x2) (min y1 y2)
                (max x1 x2) (max y1 y2))))
    (define (eachObject f)
      (send _objects foreach f))
    (define (draw)
      (send _dc clear)
      (eachObject
        (lambda (name o)
          (let* ([p (send o -> 'getPos)]
                 [x (send p get-x)]
                 [y (send p get-y)]
                 [oDC (send o getDC)])
            (send _dc draw-bitmap (send oDC get-bitmap) x y)
            (when (send _objects selected? name)
              (send _dc set-brush yellow-brush)
              (send _dc set-pen black-pen)
              (send _dc draw-ellipse x y 5 5))))))
    (define _s (new Select))
    (define _ds (new DragSelected))

    (define/override (on-subwindow-event receiver event)
      (send _s handle event)
      (send _ds handle event))))
    ;(define selectionStartCorner (make-point 0 0))
    ;(define selectionEndCorner (make-point 0 0))
    ;(define shouldDrawSelectionBox? #f)
      ;(let ([cursorPoint (make-point (send event get-x) (send event get-y))])
        ; selection
        ;(cond [(send event button-up?)
               ;(set! objectShouldDrag? #f)
               ;(set! shouldDrawSelectionBox? #f)
               ;(send objects draw)]
              ;[(send event dragging?)
               ;(cond [objectShouldDrag?
                       ;(send objects sendSelected 'setPos (add/pv cursorPoint dragOffset))
                       ;(send objects draw)]
                     ;[shouldDrawSelectionBox?
                       ;(send objects draw)
                       ;(set! selectionEndCorner cursorPoint)
                       ;(let-values ([(x1 y1 x2 y2)
                                     ;(rectFromPoints selectionStartCorner selectionEndCorner)])
                         ;(send _dc set-pen "grey70" 2 'short-dash)
                         ;(send _dc set-brush no-brush)
                         ;(send _dc draw-rectangle x1 y1 (- x2 x1) (- y2 y1)))])])))))

; A group of actions with similar functionality.
; An interaction holds state and receives events from an
; 'event source'. Interactions notify their source of
; whether the event received has been handled. If it has 
; not been, then the source will pass the event on to
; another interaction.
(define Interaction (interface () handle))

(define Select
  (class* object% (Interaction)
    (define (handle e)
      (let ([cursorPoint (make-point (send e get-x) (send e get-y))])
        (when (send e button-down?)
          (send objects setSelectedByLocation cursorPoint))))))

(define DragSelected
  (class* object% (Interaction)
    (define objectShouldDrag? #f) ; should the selected object be dragged?
    (define dragOffset (make-point 0 0)) ; offset of mouse from object location when dragging

    (define (handle e)
      (let ([cursorPoint (make-point (send e get-x) (send e get-y))])
        (when (send objects sendSelected 'inside? cursorPoint)
          (set! objectShouldDrag? #t)
          (set! dragOffset (sub/pp (send objects sendSelected 'getPos) cursorPoint)))
        (when (send e button-up?)
          (set! objectShouldDrag? #f))
        (when (and (send e dragging?) objectShouldDrag?)
          (send objects sendSelected 'setPos (add/pv cursorPoint dragOffset)))))))

#lang racket
(require racket/gui/base)
(require racket/draw)
(require "value.rkt")
(require "drawCanvas.rkt")
(require "connector.rkt")
(require "objectStore.rkt")
(require trace)

(define no-pen (new pen% [style 'transparent]))
(define no-brush (new brush% [style 'transparent]))
(define blue-brush (new brush% [color "blue"]))
(define red-pen (new pen% [color "red"] [width 2]))

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

; A group of actions with similar functionality.
; An interaction holds state and receives events from an
; 'event source'. Interactions notify their source of
; whether the event received has been handled. If it has 
; not been, then the source will pass the event on to
; another interaction.
(define Interaction (interface () handle))
    
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

(define (myMap f l)
  (send objects newObject "List" "Result List" (new resizable-bitmap-dc%))
  (send objects sendSelected 'getConnector 'list)
  (unless (empty? l)
    (send objects sendSelected 'cons (f (first l)))))

(define (m f l)
  (if (empty? l)
    '()
    (cons (f (first l)) (m f (rest l)))))
(m (curry * 2) '(1 2 3 4 5))

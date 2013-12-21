#lang racket
(require (prefix-in O: "object.rkt"))
(require "value.rkt")

(define ConsoleDC
  (class object%
    (super-new)
    (define (draw-rectangle x y w h)
      (display `(rectangle ,x ,y ,w ,h))(newline))
    (define (set-pen . _) #t)
    (define (set-brush . _) #t)
    (public set-pen set-brush draw-rectangle)))

(let* ([dc (new ConsoleDC)]
       [ob (new O:Rectangle [drawingContext dc])])
  (send ob -> 'setPos (make-point 2 3))
  (send ob -> 'setPos (make-point 2 3))
  (send ob -> 'setWidth 23))


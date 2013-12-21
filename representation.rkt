#lang racket
(provide (all-defined-out))
(require racket/draw)
(require "constraint.rkt")

(define blue-brush (new brush% [color "blue"]))
(define yellow-brush (new brush% [color "yellow"]))
(define red-pen (new pen% [color "red"] [width 2]))
(define black-pen (new pen% [color "black"] [width 2]))

(define Draw
  (class object%
    (super-new)
    (init drawingContext)
    (define _dc drawingContext)
    (define (!d s . args)
      (apply dynamic-send _dc s args))
    (public !d)))

(define Rectangle
  (class Draw
    (super-new)

    (init connector)
    (inherit !d)
    (connect connector this)

    (define _c connector)
    (define (getVar)
      _c)
    (define _name (send connector getName)) 
    (define _outline black-pen)

    (define (resolve)
      (when (send _c hasValue?)
        (let* ([s (send _c getValue)]
               [w (get-field w s)]
               [h (get-field h s)])
          (!d 'requestSize w h)
          (!d 'set-pen _outline)
          (!d 'set-brush blue-brush)
          (!d 'draw-rectangle 0 0 w h))))

    (define (reevaluate) (resolve))

    (define (setOutilne color)
      (cond [(eq? color "red")
             (set! _outline red-pen)]
            [(eq? color "black")
             (set! _outline black-pen)]))

    (define (disconnect port)
      (error "nope"))

    (define (attach . _) #t)

    (define (getName)
      _name)

    (public resolve reevaluate 
            getVar
            setOutilne
            getName
            disconnect attach)))

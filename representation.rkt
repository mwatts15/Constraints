#lang racket
(provide (all-defined-out))
(require racket/draw)
(require "constraint.rkt")

(define blue-brush (new brush% [color "blue"]))
(define yellow-brush (new brush% [color "yellow"]))
(define red-pen (new pen% [color "red"] [width 2]))
(define white-pen (new pen% [color "white"] [width 2]))
(define black-pen (new pen% [color "black"] [width 2]))

(define Draw
  (class object%
    (super-new)
    (init drawingContext)
    (define _dc drawingContext)
    (define (!d s . args)
      (apply dynamic-send _dc s args))
    (define (resolve) 
      (error "abstract"))

    (define (reevaluate) (resolve))

    (define (disconnect port)
      (error "nope"))

    (define (attach . _) #t)

    (define (inside? p) #f)

    (public-final reevaluate disconnect attach)
    (public resolve)
    (public inside?)
    (public !d)))

(define RectangleList
  (class Draw
    (super-new)
    (init connector)
    (inherit !d)
    (connect connector this)
    (define _c connector)
    (define _width 0)
    (define _height 0)
    (define/override (inside? p)
      (if (send _c hasValue?)
        (let* ([l (send _c getValue)]
               [len (length l)]
               [px (send p get-x)]
               [py (send p get-y)])
          (and (> px 0)
               (> py 0)
               (< px _width)
               (< py _height)))
        #f))
    (define/override (resolve)
      (when (send _c hasValue?)
        (!d 'set-pen black-pen)
        (!d 'set-brush blue-brush)
        (!d 'clear)
        (let* ([l (send _c getValue)]
               [sl (map (lambda (x) (format "~a" x)) l)])
          (set!-values (_width _height)
            (for/fold ([drawStart 0][height 0])
                      ([(e i) (in-indexed sl)])
              (let-values ([(tw th _ __) (!d 'get-text-extent e)])
                (!d 'requestSize (+ drawStart tw) th)
                (!d 'draw-rectangle drawStart 0 tw th)
                (!d 'set-text-foreground "white")
                (!d 'draw-text e drawStart 0)
                (values (+ drawStart tw) th)))))))))

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

    ; what do Ands mean when constraints are supposed to be true?
    ; what does an Equal constraint mean? 
    ; Shouldn't it just be a connector
    (define/override (resolve)
      (when (send _c hasValue?)
        (let* ([s (send _c getValue)]
               [w (get-field w s)]
               [h (get-field h s)])
          (!d 'requestSize w h)
          (!d 'set-pen _outline)
          (!d 'set-brush blue-brush)
          (!d 'draw-rectangle 0 0 w h))))

    (define (setOutilne color)
      (cond [(eq? color "red")
             (set! _outline red-pen)]
            [(eq? color "black")
             (set! _outline black-pen)]))

    (define/override (inside? p)
      (if (send _c hasValue?)
        (let* ([r (send _c getValue)]
               [px (send p get-x)]
               [py (send p get-y)]
               [w (get-field w r)]
               [h (get-field h r)])
          (and (> px 0) (> py 0) (< px w) (< py h)))
        #f))

    (define (getName)
      _name)

    (public getVar
            setOutilne
            getName)))

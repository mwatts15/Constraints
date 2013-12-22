#lang racket
(require (prefix-in O: "object.rkt"))
(require "value.rkt")

(define ConsoleDC
  (class object%
    (super-new)
    (define _textColorCode 0)

    (define _asciiColors
      (hash "black" 0 "Red" 1 "Green" 2 "Yellow" 3
            "Blue" 4 "Magenta" 5 "Cyan" 6 "white" 7))

    (public*
      (draw-rectangle
        (λ (x y w h)
           (display `(rectangle ,x ,y ,w ,h))))

      (draw-text
        (λ (text x y)
           (unless (eq? _textColorCode 0)
             (printf "\x1b[3~am" (modulo _textColorCode 8)))
           (display text)
           (unless (eq? _textColorCode 0)
             (printf "\x1b[39;49m"))))

      (set-text-foreground
        (λ (c)
           (when (dict-has-key? _asciiColors c)
             (set! _textColorCode (dict-ref _asciiColors c)))))
      (clear
        (λ ()
           (newline)))

      (get-text-extent 
        (λ (str)
           (values (string-length str) 1.0 0.0 0.0)))
      (set-pen (λ (a . _) #t))
      (set-brush (λ (a . _) #t))
      (get-size (λ () (values 1000 1000)))
      (requestSize (λ (a . _) #t)))))

(let* ([dc (new ConsoleDC)]
       [ob (new O:Rectangle [drawingContext dc])])
  (send ob -> 'setPos (make-point 2 3))
  (send ob -> 'setPos (make-point 2 3))
  (send ob -> 'setWidth 23))

(let* ([dc (new ConsoleDC)]
       [ob (new O:List [drawingContext dc])])
  (send ob -> 'cons 3)
  (send ob -> 'cons 4))

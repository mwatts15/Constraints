#lang racket

(provide racket->f)

(define (toInfix expr)
  (match expr
    [`(,op ,lhs ,rhs) `(,(toInfix lhs) ,op ,(toInfix rhs))]
    [x x]))

; given '(+ x y) => `((+ (res ,?z) (lhs x) (rhs y)))
; given '(+ (* a b) y) => `((+ (lhs ,?z) (rhs y) (res ,?g))
;                           (* (lhs a) (rhs b) (res ,?z))
; assumes that the associated constraints have the right names for things 
(define (racket->f expr)
  (match expr
         [`(,op ,lhs ,rhs)
           ; if lhs or rhs are symbols or constants, they just return the symbol
           ; otherwise, we have to unpack the arguments with racket->f
           (match-let ([(list lexprs lname) (racket->f lhs)]
                       [(list rexprs rname) (racket->f rhs)]
                       [resvar (gensym)])
             (list (append lexprs
                           rexprs
                           `((,op (lhs ,lname) (rhs ,rname) (res ,resvar))))
                   resvar))]
         [x (list '() x)]))

(define (mathOpt expr)
  (match (match expr
                [`(+ ,x ,x) `(* 2 ,x)]
                [`(+ ,x (+ ,x ,y)) `(+ (* 2 ,x) ,y)]
                [`(+ ,x (- ,x ,y)) `(- ,y)]
                [`(- ,x (+ ,x ,y)) y]
                [`(/ 1 (/ ,x ,y)) `(/ ,y ,x)]
                [`(/ ,x 1) x]
                [`(,(or '+ '-) ,x 0) x]
                [`(* ,x 1) x]
                [`(* ,x 0) 0]
                [`(- ,x ,x) 0]
                [`(+ (- ,x) ,y) `(- ,y ,x)]
                [`(- ,x (- ,y)) `(+ ,x ,y)]
                [`(= ,(and (not 0) x) ,y) `(= 0 (- ,y ,x))]
                ;combining terms
                [`(,(and (or '+ '-) op)
                    (* ,c1 ,x)
                    (* ,c2 ,x)) `(* (,op ,c1 ,c2) ,x)]
                [`(,(and (or '+ '-) op)
                    ,x
                    (* ,c2 ,x)) `(* (,op 1 ,c2) ,x)]
                [`(,(and (or '+ '-) op)
                    (* ,c1 ,x)
                    ,x) `(* (,op ,c1 1) ,x)]
                ; heuristic: move numbers to the outside
                [`(,(and (or '+ '-) op1)
                    (,(and (or '+ '-) op2) ,x ,(? number? y))
                    ,(? (negate number?) z))
                  `(,op2 (,op1 ,x ,z) ,y)]
                ; the 'infamous' flip rule. probably shouldn't be here...
                [`(,(and (or '+ '*) op) ,x ,y) `(,op ,y ,x)]
                [x x])
         [`(,op ,x ...) (cons op (map mathOpt x))]
         [x x]))



#lang racket

(require "constraint.rkt")
(require (prefix-in V: "value.rkt"))

; constraint representation for boolean expression:
; (x and y) or (x and (not z)) or (z and (not y))
(let ([a1 (new And)]
      [a2 (new And)]
      [a3 (new And)]
      [o1 (new Or)]
      [o2 (new Or)]
      [n1 (new Not)]
      [n2 (new Not)]
      [x (new Variable [name 'x])]
      [y (new Variable [name 'y])]
      [z (new Variable [name 'z])]
      [result (new Variable [name 'result])])
  (connectConstraints x 'out a1 'arg1)
  (connectConstraints y 'out a1 'arg2)
  (connectConstraints x 'out a2 'arg1)
  (connectConstraints z 'out n1 'arg)
  (connectConstraints n1 'out a2 'arg2)
  (connectConstraints o1 'arg1 a1 'out)
  (connectConstraints o1 'arg2 a2 'out)
  (connectConstraints z 'out a3 'arg1)
  (connectConstraints y 'out n2 'arg)
  (connectConstraints n2 'out a3 'arg2)
  (connectConstraints o2 'arg1 o1 'out)
  (connectConstraints o2 'arg2 a3 'out)
  (connectConstraints result 'out o2 'out)

  (send x setValue! false)
  (send y setValue! false)
  (send z setValue! true))

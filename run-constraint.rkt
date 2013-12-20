#lang racket

(require "bool.rkt")
(require "constraint.rkt")
(require "variables.rkt")
(require "console-rep.rkt")
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
  (connect x a1 'lhs)
  (connect y a1 'rhs)
  (connect x a2 'lhs)
  (connect z n1 'arg)
  (connectConstraints n1 'out a2 'rhs)
  (connectConstraints o1 'lhs a1 'out)
  (connectConstraints o1 'rhs a2 'out)
  (connect z a3 'lhs)
  (connect y n2 'arg)
  (connectConstraints n2 'out a3 'rhs)
  (connectConstraints o2 'lhs o1 'out)
  (connectConstraints o2 'rhs a3 'out)
  (connect result o2 'out)

  (send x setValue! false)
  (send y setValue! false)
  (send z setValue! true))

(let ([e1 (new Variable [name 'e1])]
      [e2 (new Variable [name 'e2])]
      [i1 (new Variable [name 'i1])]
      [i2 (new Variable [name 'i2])]
      [a (new Variable [name 'array])]
      [t (new Variable)]
      [arr1 (new Array)]
      [arr2 (new Array)]) 
  (connect a arr1 'array)
  (connect a arr2 'array)
  (connect e1 arr1 'value)
  (connect e2 arr2 'value)
  (connect i1 arr1 'index)
  (connect i2 arr2 'index)

  (new ConsoleRep [c a])
  (new ConsoleRep [c e1])
  (new ConsoleRep [c i1])
  (new ConsoleRep [c e2])
  (send a setValue! (vector 1 2 3 4 5 6) 'user)
  (send e1 setValue! 2 'user)
  (send i2 setValue! 5 'user)
  'done)

(let ([t (new Variable [name 'tail])]
      [h (new Variable [name 'head])]
      [l (new Variable [name 'list])]
      [th (new Variable [name 'tail-head])]
      [tt (new Variable [name 'tail-tail])]
      [theList (new Constant)]
      [firstValue (new Constant)]
      [isf (new List)]
      [istf (new List)]) 
  (connect l isf 'list)
  (connect h isf 'head)
  (connect t isf 'tail)
  (connect t istf 'list)
  (connect th istf 'head)
  (connect tt istf 'tail)
  (new ConsoleRep [c l])
  (new ConsoleRep [c h])
  (new ConsoleRep [c t])
  (new ConsoleRep [c th])
  (new ConsoleRep [c tt])
  (send t setValue! '(a b c d e f g) 'user)
  (send h setValue! 'k 'user)
  'done)

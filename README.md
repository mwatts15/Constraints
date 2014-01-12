Constraints
===========

Constraint programming. Based on the propagation of constraints described in [SICP](http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-22.html#%_sec_3.3.5).

See "cml.rkt" and "constraint-lang.rkt" for the file-reader and evaluator.
A few basic constraints are included.

Run `racket cml.rkt run-lang` for an example

Examples (not implemented!!):

Example 1:

  
    (array (index 2) (value v) (array #(a 2 3 a 5)))
    (array (index k) (value 'a) (array #(a 2 3 a 5)))
Produces:
    
    (v = 3)
    (k in (set 0 3))

Example 2:
 
    (* (rhs 2) (lhs 2) (res x^2))
    (* (rhs y) (lhs y) (res y^2))
    (+ (rhs x^2) (lhs y^2) (res 8))

Produces:

    (y^2 = 4)
    (y in (set 2 -2))


Additional constraints can be described by subclassing Constraint and augmenting the resolve method. See *-types.rkt for examples.

TODO
====
* Integrate abbreviated syntax for arithmatic constraints (e.g. `(+ (* 2 2) (* y y))` for the example above)
* Inequalities
* Multiple values on connectors

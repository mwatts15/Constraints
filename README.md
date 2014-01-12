Constraints
===========

Constraint programming. Based on the propagation of constraints described in [SICP](http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-22.html#%_sec_3.3.5).

See "cml.rkt" and "constraint-lang.rkt" for the file-reader and evaluator.
A few basic constraints are included.

Run `racket cml.rkt run-lang` for an example

Example Code (not fully implemented!!):

    (* (rhs 2) (lhs 2) (res x^2))
    (* (rhs y) (lhs y) (res y^2))
    (+ (rhs x^2) (lhs y^2) (res 8))

Produces:

    (= y^2 4)
    (in y (set 2 -2))

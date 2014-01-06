#lang racket

(require "connector.rkt")

(define (graphvizOut . verts)
  (define names (make-hash))
  (define (getName c)
    ((compose (lambda (n) (regexp-replace "-" n "_"))
              (curry format "~a"))
     (dict-ref! names c (gensym (send c getName)))))

  (printf "graph g {~n")
  (printf "overlap=false~n")
  (for ([e (apply networkEdges verts)])
    (when (is-a? (car e) ConnectorObserver)
      (printf "~a [shape=box]~n" (getName (car e))))
    (when (is-a? (cdr e) ConnectorObserver)
      (printf "~a [shape=box]~n" (getName (cdr e))))
    (printf "~a--~a~n" (getName (first e)) (getName (second e))))
  (printf "}~n"))


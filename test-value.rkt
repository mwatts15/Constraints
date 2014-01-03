#lang racket

(require "value.rkt")
(require "test-util.rkt")

(define tests
  (list
    (test-suite "World"
                (test-case "newly placed object is there"
                           (let ([w (new World)])
                             (send w placeObject 5 'aLocation)
                             (check-equal? (send w getObjectsAt 'aLocation) '(5))))
                (test-case "place multiple objects"
                           (let ([w (new World)])
                             (send w placeObject 5 'aLocation)
                             (send w placeObject 2 'aLocation)
                             (send w placeObject 1 'aLocation)
                             (check-equal? (sort (send w getObjectsAt 'aLocation) <) '(1 2 5))))
                (test-case "re-place object same place"
                           (let ([w (new World)])
                             (send w placeObject 5 'aLocation)
                             (send w placeObject 5 'aLocation)
                             (check-equal? (send w getObjectsAt 'aLocation) '(5))))
                (test-case "re-place object"
                           (let ([w (new World)])
                             (send w placeObject 5 'aLocation)
                             (send w placeObject 5 'newLocation)
                             (check-equal? (send w getObjectsAt 'aLocation) '())
                             (check-equal? (send w getObjectsAt 'newLocation) '(5))))
                (test-case "move object"
                           (let ([w (new World)])
                             (send w placeObject 5 'aLocation)
                             (send w moveObject 5 'newLocation)
                             (check-equal? (send w getObjectsAt 'aLocation) '())
                             (check-equal? (send w getObjectsAt 'newLocation) '(5))))
                (test-case "remove objects"
                           (let ([w (new World)])
                             (send w placeObject 5 'aLocation)
                             (send w removeObjectsAt 'aLocation)
                             (check-equal? (send w getObjectsAt 'aLocation) '())))
                (test-case "move object same place"
                           (let ([w (new World)])
                             (send w placeObject 5 'aLocation)
                             (send w moveObject 5 'aLocation)
                             (check-equal? (send w getObjectsAt 'aLocation) '(5)))))
    (test-suite "Range"
                (test-case "addition"
                           (let ([r1 (new Range (start 2) (end 3))]
                                 [r2 (new Range (start 5) (end 7))])
                             (let ([r3 (send r1 add r2)])
                               (check-eq? 7 (get-field start r3))
                               (check-eq? 10 (get-field end r3))))))))
(for ([t tests])
  (run-tests t 'verbose))

#lang racket

(require "value.rkt")
(require "test-util.rkt")

(define tests
  (list
    (test-suite "isConsistentWith?"
      (test-suite "World"
        (test-case "new emptys are inconsistent"
          (let ([w (new World)]
                [v (new World)])
            (check-false (send w isConsistentWith? v))))
        (test-case "distinct worlds are inconsistent"
          (let ([w (new World)]
                [v (new World)])
            (send w placeObject 'anObject 'aLocation)
            (send v placeObject 'anObject 'aLocation)
            (check-false (send w isConsistentWith? v))))
        (test-case "copies are consistent"
          (let* ([w (new World)]
                 [v (new World [oldWorld w])])
            (check-true (send w isConsistentWith? v))))
        (test-case "copies with changes are consistent"
          (let ([w (new World)])
            (send w placeObject 'anObject 'aLocation)
            (let ([v (new World [oldWorld w])])
              (check-true (send w isConsistentWith? v)))))
        (test-case "movements are inconsistent"
          (let ([w (new World)])
            (send w placeObject 'anObject 'aLocation)
            (let ([v (new World [oldWorld w])])
              (send v moveObject 'anObject 'someOtherLocation)
              (check-false (send w isConsistentWith? v)))))
        (test-case "co-locations are consistent"
          (let ([w (new World)])
            (send w placeObject 'anObject 'aLocation)
            (let ([v (new World [oldWorld w])])
              (send v placeObject 'anotherObject 'aLocation)
              (check-true (send w isConsistentWith? v)))))
        (test-case "adding other objects is consistent"
          (let ([w (new World)])
            (send w placeObject 'anObject 'aLocation)
            (let ([v (new World [oldWorld w])])
              (send v placeObject 'anotherObject 'someOtherLocation)
              (check-true (send w isConsistentWith? v)))))))
    (test-suite "World"
      (test-case "newly placed object is there"
        (let ([w (new World)])
          (send w placeObject 'anObject 'aLocation)
          (check-equal? (send w getObjectsAt 'aLocation) '(anObject))))
      (test-case "place multiple objects"
        (let ([w (new World)])
          (send w placeObject 'anObject 'aLocation)
          (send w placeObject 'anotherObject 'aLocation)
          (send w placeObject 'someObject 'aLocation)
          (check-equal? (list->set (send w getObjectsAt 'aLocation))
                        (set 'someObject 'anotherObject 'anObject))))
      (test-case "re-place object same place"
        (let ([w (new World)])
          (send w placeObject 'anObject 'aLocation)
          (send w placeObject 'anObject 'aLocation)
          (check-equal? (send w getObjectsAt 'aLocation) '(anObject))))
      (test-case "re-place object"
        (let ([w (new World)])
          (send w placeObject 'anObject 'aLocation)
          (send w placeObject 'anObject 'newLocation)
          (check-equal? (send w getObjectsAt 'aLocation) '())
          (check-equal? (send w getObjectsAt 'newLocation) '(anObject))))
      (test-case "move object"
        (let ([w (new World)])
          (send w placeObject 'anObject 'aLocation)
          (send w moveObject 'anObject 'newLocation)
          (check-equal? (send w getObjectsAt 'aLocation) '())
          (check-equal? (send w getObjectsAt 'newLocation) '(anObject))))
      (test-case "remove objects"
        (let ([w (new World)])
          (send w placeObject 'anObject 'aLocation)
          (send w removeObjectsAt 'aLocation)
          (check-equal? (send w getObjectsAt 'aLocation) '())))
      (test-case "init from world"
        (let ([w (new World)])
          (send w placeObject 'anObject 'aLocation)
          (let ([w-with-anObject (new World [oldWorld w])])
            (check-equal? (send w-with-anObject getObjectsAt 'aLocation) '(anObject)))))
      (test-case "move object same place"
        (let ([w (new World)])
          (send w placeObject 'anObject 'aLocation)
          (send w moveObject 'anObject 'aLocation)
          (check-equal? (send w getObjectsAt 'aLocation) '(anObject)))))
    (test-suite "Range"
      (test-case "addition"
        (let ([r1 (new Range (start 2) (end 3))]
              [r2 (new Range (start 5) (end 7))])
          (let ([r3 (send r1 add r2)])
            (check-eq? 7 (get-field start r3))
            (check-eq? 10 (get-field end r3))))))))
(for ([t tests])
  (run-tests t 'verbose))

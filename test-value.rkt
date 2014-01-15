#lang racket

(require "value.rkt"
         "test-util.rkt")

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
      (test-suite "addition"
        (test-case "range+range"
          (let ([r1 (Range 2 3)]
                [r2 (Range 5 7)])
            (check-equal? (Range 7 10) (Range+Range r1 r2))))
        (test-case "range+number"
          (let* ([r (Range 0 +inf.0)]
                 [rp (Range+number r 9.0)])
            (check-equal? rp (Range 9.0 +inf.0))))
        (test-case "Range+number bad argument"
          (check-exn (const #t) (thunk (Range+number (Range 1 2) 'horse))))
        (test-case "Range+Range bad argument"
          (check-exn (const #t) (thunk (Range+Range (Range 1 2) 'horse)))))
      (test-suite "subset"
        (test-case "smaller in larger"
          (let ([r1 (Range 8.0 10.0)]
                [r2 (Range 8.0 +inf.0)]) 
            (check-true (r1 . isSubsetOf? . r2))))
        (test-case "overlapping"
          (let ([r1 (Range 7.0 10.0)]
                [r2 (Range 8.0 20)]) 
            (check-false (r1 . isSubsetOf? . r2))))
        (test-case "same"
          (let ([r1 (Range 8.0 20.0)]
                [r2 (Range 8.0 20.0)]) 
            (check-true (r1 . isSubsetOf? . r2))))
        (test-case "larger in smaller"
          (let ([r1 (Range 7.0 10.0)]
                [r2 (Range 8.0 20)]) 
            (check-false (r2 . isSubsetOf? . r1)))))
      (test-case "intersect overlapping"
        (let ([r1 (Range -2.0 10.0)]
              [r2 (Range 8.0 +inf.0)]) 
          (check-equal? (intersect r1 r2) (Range 8.0 10.0))))
      (test-case "intersect kissing"
        (let ([r1 (Range -2.0 8.0)]
              [r2 (Range 8.0 +inf.0)]) 
          (check-equal? (intersect r2 r1) (Singleton 8.0))
          (check-equal? (intersect r1 r2) (Singleton 8.0))))
      (test-case "intersect disjoint"
        (let ([r1 (Range -2.0 6.0)]
              [r2 (Range 8.0 +inf.0)]) 
            (check-equal? (intersect r2 r1) EmptySet)
            (check-equal? (intersect r1 r2) EmptySet)))
      (test-case "start <= end"
        (check-exn (const #t) (thunk (Range 20 2)))))))
(for ([t tests])
  (run-tests t 'verbose))

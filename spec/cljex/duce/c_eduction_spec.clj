(ns cljex.duce.c-eduction-spec
  "Eduction is like sequence without the comp function, but not quite.
   It takes any number of arguments, with the last always being the
   collection to consume."
  (:require [speclj.core :refer :all]))

(describe "Eduction"
  (with-stubs)

  (it "is like sequence without comp"
    (should= [2 4 6 8 10] (sequence (comp (map inc) (filter even?)) (range 10)))
    (should= [2 4 6 8 10] (eduction (map inc) (filter even?) (range 10)))

    (should= [-1 1 3 5 7] (sequence (comp (map dec) (filter odd?)) (range 10)))
    (should= [-1 1 3 5 7] (eduction (map dec) (filter odd?) (range 10)))

    (should= [1 3 5 7 9] (sequence (comp (filter even?) (map inc)) (range 10)))
    (should= [1 3 5 7 9] (eduction (filter even?) (map inc) (range 10)))

    (should= [0 2 4 6 8] (sequence (comp (filter odd?) (map dec)) (range 10)))
    (should= [0 2 4 6 8] (eduction (filter odd?) (map dec) (range 10))))

  (it "limits the sequence at the begging and the end"
    (let [xform (comp (filter odd?) (map dec))]
      (should= [0 2 4 6 8] (sequence (comp (take 10) xform) (range)))
      (should= [0 2 4 6 8] (eduction (take 10) xform (range)))

      (should= [0 2 4 6 8 10 12 14 16 18] (sequence (comp xform (take 10)) (range)))
      (should= [0 2 4 6 8 10 12 14 16 18] (eduction xform (take 10) (range)))))

  (it "has no initial value"
    (sequence (map (stub :sequence)) (range 10))
    (should-have-invoked :sequence {:times 1})
    (eduction (map (stub :eduction)) (range 10))
    (should-have-invoked :eduction {:times 0}))

  (it "evaluates the collection every time"
    (let [result (sequence (map (stub :sequence)) (range 10))]
      (should-have-invoked :sequence {:times 1})
      (doall 0 result)
      (should-have-invoked :sequence {:times 10})
      (doall result)
      (should-have-invoked :sequence {:times 10}))

    (let [result (eduction (map (stub :eduction)) (range 10))]
      (should-have-invoked :eduction {:times 0})
      (doall result)
      (should-have-invoked :eduction {:times 10})
      (doall result)
      (should-have-invoked :eduction {:times 20})))

  )

(ns cljex.duce.c-eduction-spec
  "Eduction is basically just transduce without the comp function.
   It takes any number of arguments, with the last always being the
   collection to transduce on."
  (:require [speclj.core :refer :all]))

(def xf-inc-even? (comp (map inc) (filter even?)))
(def xf-odd?-dec (comp (filter odd?) (map dec)))

(describe "Eduction"
  (it "is like transduce without comp"
    (should= [2 4 6 8 10] (transduce (comp (map inc) (filter even?)) conj (range 10)))
    (should= [2 4 6 8 10] (eduction (map inc) (filter even?) (range 10)))

    (should= [-1 1 3 5 7] (transduce (comp (map dec) (filter odd?)) conj (range 10)))
    (should= [-1 1 3 5 7] (eduction (map dec) (filter odd?) (range 10)))

    (should= [1 3 5 7 9] (transduce (comp (filter even?) (map inc)) conj (range 10)))
    (should= [1 3 5 7 9] (eduction (filter even?) (map inc) (range 10)))

    (should= [0 2 4 6 8] (transduce (comp (filter odd?) (map dec)) conj (range 10)))
    (should= [0 2 4 6 8] (eduction (filter odd?) (map dec) (range 10))))

  (it "limits the sequence at the begging and the end"
    (should= [0 2 4 6 8] (transduce (comp (take 10) xf-odd?-dec) conj (range)))
    (should= [0 2 4 6 8] (eduction (take 10) xf-odd?-dec (range)))

    (should= [0 2 4 6 8 10 12 14 16 18] (transduce (comp xf-odd?-dec (take 10)) conj (range)))
    (should= [0 2 4 6 8 10 12 14 16 18] (eduction xf-odd?-dec (take 10) (range))))

  (it "has no initial value"
    (should= [10 2 4 6 8 10] (into [10] (eduction xf-inc-even? (range 10))))
    (should= (+ 10 2 4 6 8 10) (reduce + 10 (eduction xf-inc-even? (range 10))))))
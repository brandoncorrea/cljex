(ns duce.b-transduce-spec
  "Transducers are functions that create other reducible functions,
   which can then be applied over collections."
  (:require [speclj.core :refer :all]))

; Increment all the numbers, then take those that are even
(def xf-inc-even? (comp (map inc) (filter even?)))

; Take all the even numbers, then increment them
(def xf-even?-inc (comp (filter even?) (map inc)))

; Decrement all the numbers, then take those that are odd
(def xf-dec-odd? (comp (map dec) (filter odd?)))

; Take all the odd numbers, then decrement them
(def xf-odd?-dec (comp (filter odd?) (map dec)))

(describe "Transduce"
  (it "breakdown"
    ; Transduce wraps f (conj) with a given xform (map inc)
    (should= [1 2 4 6 8] (transduce (map inc) conj [1 2] [3 5 7]))
    ; ... which is equivalent to this reduce form
    (should= [1 2 4 6 8] (reduce ((map inc) conj) [1 2] [3 5 7]))

    (let [xform (map inc)]
      ; Same as above, except (map inc) is now in xform
      (should= [1 2 4 6 8] (transduce xform conj [1 2] [3 5 7]))
      (should= [1 2 4 6 8] (reduce (xform conj) [1 2] [3 5 7]))

      ; Step is now an arity-3 function
      ; that is reduced over each item in the collection
      (let [step (xform conj)]
        ; Item 1, increment and conj
        (should= [1 2 4] (step [1 2] 3))
        ; Item 2, increment and conj
        (should= [1 2 4 6] (step [1 2 4] 5))
        ; Item 3, increment and conj
        (should= [1 2 4 6 8] (step [1 2 4 6] 7))
        ; Arity-1 Completion performs any final operations on the result, if any
        (should= [1 2 4 6 8] (step [1 2 4 6 8])))))

  (it "is like reduce, but accepts a function that wraps another function"
    (should= (+ 10 3 5 9) (transduce (map inc) + 10 [2 4 8]))
    (should= (+ 10 3 5 9) (reduce ((map inc) +) 10 [2 4 8])))

  (it "Invokes f with arity 0 when no initial value is provided"
    (should= (+ 3 5 9) (transduce (map inc) + [2 4 8]))
    (should= (+ 3 5 9) (reduce ((map inc) +) (+) [2 4 8])))

  (it "sums the result of applying a transducer"
    (should= (+ 2 4 6 8 10) (transduce xf-inc-even? + (range 10)))
    (should= (+ -1 1 3 5 7) (transduce xf-dec-odd? + (range 10)))
    (should= (+ 1 3 5 7 9) (transduce xf-even?-inc + (range 10)))
    (should= (+ 0 2 4 6 8) (transduce xf-odd?-dec + (range 10))))

  (it "transduce a collection into a new collection"
    (should= [2 4 6 8 10] (transduce xf-inc-even? conj (range 10)))
    (should= [-1 1 3 5 7] (transduce xf-dec-odd? conj (range 10)))
    (should= [1 3 5 7 9] (transduce xf-even?-inc conj (range 10)))
    (should= [0 2 4 6 8] (transduce xf-odd?-dec conj (range 10))))

  (it "limits the sequence at the begging and the end"
    (should= [0 2 4 6 8] (transduce (comp (take 10) xf-odd?-dec) conj (range)))
    (should= [0 2 4 6 8 10 12 14 16 18] (transduce (comp xf-odd?-dec (take 10)) conj (range))))

  (it "accepts an initial value"
    (should= [10 2 4 6 8 10] (transduce xf-inc-even? conj [10] (range 10)))
    (should= (+ 10 2 4 6 8 10) (transduce xf-inc-even? + 10 (range 10))))

  (it "transforms a collection lazily"
    (should= '(2 4 6 8 10) (sequence xf-inc-even? (range 10)))
    (should= '(-1 1 3 5 7) (sequence xf-dec-odd? (range 10)))
    (should= '(1 3 5 7 9) (sequence xf-even?-inc (range 10)))
    (should= '(0 2 4 6 8) (sequence xf-odd?-dec (range 10))))

  (it "transforms a collection eagerly"
    (should= [2 4 6 8 10] (into [] xf-inc-even? (range 10)))
    (should= [-1 1 3 5 7] (into [] xf-dec-odd? (range 10)))
    (should= [1 3 5 7 9] (into [] xf-even?-inc (range 10)))
    (should= [0 2 4 6 8] (into [] xf-odd?-dec (range 10)))))

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

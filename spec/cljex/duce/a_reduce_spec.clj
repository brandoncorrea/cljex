(ns cljex.duce.a-reduce-spec
  "Transducers and reducers are very similar.
   Here are a few examples of how reduce works."
  (:require [speclj.core :refer :all]))

(defn throwing-fn [] (throw "Some Error"))
(defn arity-zero [] :nothing!)

(describe "Reduce"

  (it "accepts arity zero function when coll is empty"
    (should= :nothing! (reduce arity-zero [])))

  (it "does not call the function if the collection only has one item"
    (should= 1 (reduce throwing-fn [1])))

  (it "does not invoke the function if there is an initial value provided with an empty collection"
    (should= 1 (reduce throwing-fn 1 [])))

  (it "results in the f(init, first) when there is an inital value and only one item in coll"
    (should= 5 (reduce + 2 [3])))

  (it "requires a function that supports arity-2"
    (should= 0 (reduce + []))
    (should= [] (reduce conj []))
    (should= 7 (reduce + [1 2 4]))
    (should= 8 (reduce * [1 2 4]))
    (should= [:a :b 1 2 'c 'd] (reduce into [[:a :b] [1 2] ['c 'd]]))
    (letfn [(assoc-even? [m n] (assoc m n (even? n)))]
      (reduce assoc-even? {} [1 2 4])))

  (it "supports initial values"
    (should= 17 (reduce + 10 [1 2 4]))
    (should= [1 2 4] (reduce conj [] [1 2 4]))
    (should= [:c :d :a :b 1 2] (reduce into [:c :d] [[:a :b] [1 2]])))

  (it "can filter a collection"
    (should= [0 2 4 6 8] (reduce #(if (even? %2) (conj %1 %2) %1) [] (range 10))))

  (it "can map over values"
    (should= [1 2 3] (reduce #(conj %1 (inc %2)) [] (range 3)))))

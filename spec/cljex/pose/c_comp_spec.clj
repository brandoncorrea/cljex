(ns cljex.pose.c-comp-spec
  "Think of this one like a backwards thread macro:
   (comp z y x) => #(-> % x y z)"
  (:require [speclj.core :refer :all]))

(describe "comp"

  (it "no arguments results in the identity function"
    (should= identity (comp)))

  (it "one argument results in that same argument"
    (should= + (comp +))
    (should= every? (comp every?))
    (should= "boo!" (comp "boo!")))

  (it "is basically a function that reads like a backwards thread form"
    (let [f       (comp even? inc :a)
          thing-1 {:a 3}
          thing-2 {:a 4}]
      (should= true (f thing-1))
      (should= false (f thing-2))
      (should= (-> thing-1 :a inc even?) (f thing-1))
      (should= (-> thing-2 :a inc even?) (f thing-2))))

  (it "order matters"
    (let [add-ten            #(+ 10 %)
          halve              #(/ % 2)
          add-ten-then-halve (comp halve add-ten)
          halve-then-add-ten (comp add-ten halve)]
      (should= 15 (add-ten-then-halve 20))
      (should= 15 (halve (add-ten 20)))
      (should= 20 (halve-then-add-ten 20))
      (should= 20 (add-ten (halve 20))))))

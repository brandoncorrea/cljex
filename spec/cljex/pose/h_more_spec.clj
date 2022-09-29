(ns cljex.pose.h-more-spec
  "Composing functions create new values.
   References to the functions being composed are lost"
  (:require [speclj.core :refer :all]))

(defn times-two [n] (* 2 n))
(defn times-six [n] (times-two (* 3 n)))

(describe "A Composed Function"

  (it "is defined outside the scope of with-redefs"
    (let [plus-three (comp inc inc inc)]
      (with-redefs [inc dec]
        (should= 8 (plus-three 5)))))

  (it "is defined within the scope of with-redefs"
    (with-redefs [inc dec]
      (let [plus-three (comp inc inc inc)]
        (should= 2 (plus-three 5)))))

  (it "can redefined calls within other functions, but not those in composed (and pre-evaluated) functions"
    (let [times-twelve (comp times-two times-six)]
      (should= (times-twelve 2) (times-two (times-six 2)))
      (should= (times-twelve 2) (* 2 (* 2 (* 3 2))))
      (should= (times-twelve 2) 24)
      (with-redefs [times-two inc]
        (should= (times-twelve 2) (* 2 (inc (* 3 2))))
        (should= (times-twelve 2) 14))))

  (it "can work together with other composers"
    (let [bogus (some-fn
                  (comp (partial every? true?)
                        (juxt pos? even?)
                        second
                        (partial map :a))
                  (constantly :bogus))]
      (should= (bogus [{:a 1} {:a 2}]) true)
      (should= (bogus [{:a 2} {:a 1}]) :bogus)))

  (it "is sometimes more obscure than a plain 'ol function"
    (let [ab-1  (comp (partial reduce +) (juxt :a :b))
          ab-2  (fn [m] (+ (:a m) (:b m)))
          thing {:a 1 :b 2}]
      (should= 3 (ab-1 thing))
      (should= 3 (ab-2 thing))
      (should= 3 (+ (:a thing) (:b thing))))))

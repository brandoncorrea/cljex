(ns cljex.pose.i-more-spec
  "Composing functions create new values.
   References to the functions being composed are lost."
  (:require [speclj.core :refer :all]))

(defn times-two [n] (* 2 n))
(defn times-six [n] (times-two (* 3 n)))
(def missing-intellisense (comp :c :b :a))

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
      (should= (times-twelve 2) 24)
      (should= (times-twelve 2) (* 2 (times-two (* 3 2))))
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
      (should= 3 (+ (:a thing) (:b thing)))))

  (it "is best for map and filter -type functions"
    (let [things [{:a {:b {:c 1}}}
                  {:a {:b {:c 2}}}
                  {:a {:b {:c 3}}}]]
      (should= [1 2 3] (map #(-> % :a :b :c) things))
      (should= [1 2 3] (map (comp :c :b :a) things))
      (should= [{:a {:b {:c 2}}}] (filter #(-> % :a :b :c even?) things))
      (should= [{:a {:b {:c 2}}}] (filter (comp even? :c :b :a) things))))

  (it "is unnecessary when only one parameter is provided"
    (let [things [{:a 1} {:a 2} {:a 3}]]
      (should= [1 2 3] (map (partial :a) things))
      (should= [1 2 3] (map (comp :a) things))
      (should= [1 2 3] (map :a things))))
  )

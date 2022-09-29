(ns cljex.pose.d_juxt-spec
  "A prettier way of doing [(f1 m) (f2 m) (f3 m) ...]"
  (:require [speclj.core :refer :all]))

(describe "juxt"

  (it "just creates an array of function results"
    (let [j-identity (juxt identity)
          f-a        (juxt :a)
          f-ab       (juxt :a :b)
          f-bac      (juxt :b :a :c)
          f-complex  (juxt (comp str :b) :a (comp symbol name :c))
          thing      {:a :x :b :y :c :z}]
      (should= [1] (j-identity 1))
      (should= [:x] (f-a thing))
      (should= [:x :y] (f-ab thing))
      (should= [:y :x :z] (f-bac thing))
      (should= [":y" :x 'z] (f-complex thing))))

  (it "takes N arguments, where N is an arity shared by all the juxtaposed functions"
    (let [n-func (juxt + * list)]
      (should= [9 24 '(2 3 4)] (n-func 2 3 4))))

  (it "is useful for sorting by multiple values"
    (let [things [{:a 1 :b 2} {:a 0 :b 2} {:a 1 :b 1}]]
      (should= [{:a 0 :b 2} {:a 1 :b 1} {:a 1 :b 2}] (sort-by (juxt :a :b) things))))

  (it "looks like a mapv or a [(:a m) (:b m) (:c m)]"
    (let [thing {:a 1 :b 2 :c 3}
          abc   (juxt :a :b :c)]
      (should= [1 2 3] (mapv #(% thing) [:a :b :c]))
      (should= [1 2 3] [(:a thing) (:b thing) (:c thing)])
      (should= [1 2 3] (abc thing))
      (should= true (vector? (abc thing)))))

  (it "perform many operations with many functions..."
    (let [[atom-a atom-b atom-c] (repeatedly #(atom 0))
          mod-a! #(reset! atom-a (inc %))
          mod-b! #(reset! atom-b (dec %))
          mod-c! #(reset! atom-c %)]
      (mod-a! 4)
      (mod-b! 4)
      (mod-c! 4)
      (should= 5 @atom-a)
      (should= 3 @atom-b)
      (should= 4 @atom-c)))

  (it "...or consolidate those calls into a juxt function"
    (let [[atom-a atom-b atom-c] (repeatedly #(atom 0))
          mod-a!   #(reset! atom-a (inc %))
          mod-b!   #(reset! atom-b (dec %))
          mod-c!   #(reset! atom-c %)
          mod-abc! (juxt mod-a! mod-b! mod-c!)]
      (mod-abc! 4)
      (should= 5 @atom-a)
      (should= 3 @atom-b)
      (should= 4 @atom-c))))

(ns cljex.duce.f-exercise
  "Each context has several test cases.
   Fill out the blank fields to make them pass: ..., xform, xform*, coll, and f.

   TIP: If you get stuck, try solving the threading spec first.
   The transducer forms will usually look very similar to this."
  (:require [clojure.string :as s]
            [speclj.core :refer :all]))

; Helper Functions
(defn fruit? [f] (= :fruit (:kind f)))
(defn veg? [f] (= :vegetable (:kind f)))
(defn meat? [f] (= :meat (:kind f)))
(defn expensive? [f] (> (:cost f) 3))
(defn short-string? [s] (< (count s) 7))
(defn has-space? [s] (s/includes? s " "))
(defn split-space [s] (s/split s #" "))

; Test Data
(def foods
  [{:name "Banana" :perishable? true :kind :fruit :cost 1}
   {:name "Canned Pineapple" :perishable? false :kind :fruit :cost 2}
   {:name "Celery" :perishable? true :kind :vegetable :cost 3}
   {:name "Lettuce" :perishable? true :kind :vegetable :cost 6}
   {:name "Canned Peas" :perishable? false :kind :vegetable :cost 2}
   {:name "Vienna Sausages" :perishable? false :kind :meat :cost 3}
   {:name "Ground Beef" :perishable? true :kind :meat :cost 6}
   {:name "Chicken Wings" :perishable? true :kind :meat :cost 5}])

(describe "Exercise"

  #_(context "Names of all the :perishable? :fruit"
      (it "threading"
        ;(should= ["Banana"] (->> foods ...))
        )
      (it "sequence"
        ;(should= ["Banana"] (sequence xform foods))
        )
      (it "into"
        ;(should= ["Banana"] (into coll xform foods))
        )
      )

  #_(context "Set of all the different :kinds of food"
      (it "threading"
        ;(should= #{:fruit :vegetable :meat} (->> foods ...))
        )
      (it "into"
        ;(should= #{:fruit :vegetable :meat} (into coll xform foods))
        )
      )

  #_(context "Collection of the costs of :vegetables"
      (it "threading"
        ;(should= [3 6 2] (->> foods ...))
        )
      (it "sequence"
        ;(should= [3 6 2] (sequence xform foods))
        )
      (it "into"
        ;(should= [3 6 2] (into coll xform foods))
        )
      (it "eduction"
        ;(should= [3 6 2] (eduction xform* foods))
        )
      )

  #_(context "Total cost of all :vegetables"
      (it "threading"
        ;(should= 11 (->> foods ...))
        )
      (it "transduce"
        ;(should= 11 (transduce xform f foods))
        )
      )

  #_(context "Total cost of all the :perishable? :vegetables"
      (it "threading"
        ;(should= 9 (->> foods ...))
        )
      (it "transduce"
        ;(should= 9 (transduce xform f foods))
        )
      )

  #_(context "Names of expensive? :meat products"
      (it "threading"
        ;(should= ["Ground Beef" "Chicken Wings"] (->> foods ...))
        )
      (it "into"
        ;(should= ["Ground Beef" "Chicken Wings"] (into coll xform foods))
        )
      (it "sequence"
        ;(should= ["Ground Beef" "Chicken Wings"] (sequence xform foods))
        )
      (it "eduction"
        ;(should= ["Ground Beef" "Chicken Wings"] (eduction xform foods))
        )
      )

  #_(context "First letter of all :perishable? foods with short names"
      (it "threading"
        ;(should= [\B \C] (->> foods ...))
        )
      (it "sequence"
        ;(should= [\B \C] (sequence xform foods))
        )
      (it "eduction"
        ;(should= [\B \C] (eduction xform foods))
        )
      )

  #_(context "First letter of all :perishable? foods with short names, except put into a string"
      (it "threading"
        ;(should= "BC" (->> foods ...))
        )
      (it "transduce"
        ;(should= "BC" (transduce xform f foods))
        )
      )

  #_(context "Last words in inexpensive foods that contain whitespace in their name"
      (it "threading"
        ;(should= ["Pineapple" "Peas" "Sausages"] (->> foods ...))
        )
      (it "sequence"
        ;(should= ["Pineapple" "Peas" "Sausages"] (sequence xform foods))
        )
      )
  )

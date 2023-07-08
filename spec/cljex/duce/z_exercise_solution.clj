(ns cljex.duce.z-exercise-solution
  "No peeking until you've attempted the exercises!"
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

; --- Transducers

(def xf-perishables (filter :perishable?))
(def xf-names (map :name))

(def xf-perishable-fruit-names
  (comp
    xf-perishables
    (filter fruit?)
    xf-names))

(def xf-veg-costs
  (comp
    (filter veg?)
    (map :cost)))

(def xf-perishable-veg-costs (comp xf-perishables xf-veg-costs))
(def xf-expensive-meat-names
  (comp
    (filter meat?)
    (filter expensive?)
    xf-names))

(def xf-perishable-first-letters
  (comp
    xf-perishables
    xf-names
    (filter short-string?)
    (map first)))

(def xf-inexpensive-last-words
  (comp
    (remove expensive?)
    xf-names
    (filter has-space?)
    (map split-space)
    (map last)))

; ^^^ Transducers

; --- Threads

(defn perishables [foods] (filter :perishable? foods))
(defn names [foods] (map :name foods))

(defn perishable-fruit-names [foods]
  (->> foods
       perishables
       (filter fruit?)
       names))

(defn veg-costs [foods]
  (->> foods (filter veg?) (map :cost)))

(defn perishable-veg-costs [foods]
  (->> foods perishables veg-costs))

(defn expensive-meat-names [foods]
  (->> foods
       (filter expensive?)
       (filter meat?)
       names))

(defn perishables-first-letters [foods]
  (->> foods
       perishables
       names
       (filter short-string?)
       (map first)))

(defn inexpensive-last-words [foods]
  (->> foods
       (remove expensive?)
       names
       (filter has-space?)
       (map split-space)
       (map last)))

; ^^^ Threads

(describe "Exercise"

  (context "Names of all the :perishable? :fruit"
    (it "threading"
      (should= ["Banana"] (perishable-fruit-names foods)))
    (it "sequence"
      (should= ["Banana"] (sequence xf-perishable-fruit-names foods)))
    (it "into"
      (should= ["Banana"] (into [] xf-perishable-fruit-names foods))))

  (context "Set of all the different :kinds of food"
    (it "threading"
      (should= #{:fruit :vegetable :meat} (set (map :kind foods))))
    (it "into"
      (should= #{:fruit :vegetable :meat} (into #{} (map :kind) foods))))

  (context "Collection of the costs of :vegetables"
    (it "threading"
      (should= [3 6 2] (veg-costs foods)))
    (it "sequence"
      (should= [3 6 2] (sequence xf-veg-costs foods)))
    (it "into"
      (should= [3 6 2] (into [] xf-veg-costs foods)))
    (it "eduction"
      (should= [3 6 2] (eduction (filter veg?) (map :cost) foods))))

  (context "Total cost of all :vegetables"
    (it "threading"
      (should= 11 (reduce + (veg-costs foods))))
    (it "transduce"
      (should= 11 (transduce xf-veg-costs + foods))))

  (context "Total cost of all the :perishable? :vegetables"
    (it "threading"
      (should= 9 (reduce + (perishable-veg-costs foods))))
    (it "transduce"
      (should= 9 (transduce xf-perishable-veg-costs + foods))))

  (context "Names of expensive? :meat products"
    (it "threading"
      (should= ["Ground Beef" "Chicken Wings"] (expensive-meat-names foods)))
    (it "into"
      (should= ["Ground Beef" "Chicken Wings"] (into [] xf-expensive-meat-names foods)))
    (it "sequence"
      (should= ["Ground Beef" "Chicken Wings"] (sequence xf-expensive-meat-names foods)))
    (it "eduction"
      (should= ["Ground Beef" "Chicken Wings"] (eduction xf-expensive-meat-names foods))))

  (context "First letter of all :perishable? foods with short names"
    (it "threading"
      (should= [\B \C] (perishables-first-letters foods)))
    (it "sequence"
      (should= [\B \C] (sequence xf-perishable-first-letters foods)))
    (it "eduction"
      (should= [\B \C] (eduction xf-perishable-first-letters foods))))

  (context "First letter of all :perishable? foods with short names, except put into a string"
    (it "threading"
      (should= "BC" (reduce str (perishables-first-letters foods))))
    (it "transduce"
      (should= "BC" (transduce xf-perishable-first-letters str foods))))

  (context "Last words in inexpensive foods that contain whitespace in their name"
    (it "threading"
      (should= ["Pineapple" "Peas" "Sausages"] (inexpensive-last-words foods)))
    (it "sequence"
      (should= ["Pineapple" "Peas" "Sausages"] (sequence xf-inexpensive-last-words foods)))))

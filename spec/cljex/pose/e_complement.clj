(ns cljex.pose.e-complement
  "You're looking fabulous today 😎! Basically, the logical NOT composer."
  (:require [clojure.string :refer [blank?]]
            [speclj.core :refer :all]))

(comment "Quite literally just (not (f ...))"
  (defn complement
    "Takes a fn f and returns a fn that takes the same arguments as f,
     has the same effects, if any, and returns the opposite truth value."
    [f]
    (fn
      ([] (not (f)))
      ([x] (not (f x)))
      ([x y] (not (f x y)))
      ([x y & zs] (not (apply f x y zs))))))

(describe "Complement"

  (it "is NOT blank"
    (let [not-blank? (complement blank?)]
      (should= true (not-blank? "Aw, shucks!"))
      (should= false (blank? "Aw, shucks!"))))

  (it "complements a complement"
    (let [not-blank?     (complement blank?)
          NOT-not-blank? (complement not-blank?)]
      (should= true (not-blank? "Nawww"))
      (should= false (NOT-not-blank? "Nawww"))
      (should= false (blank? "Nawww"))))

  (it "casts to a boolean (because of the 'not' part)"
    (let [unidentified? (complement :name)
          dory          {:kind :blue-tang}
          nemo          {:kind :clownfish :name "Nemo"}]
      (should-be-nil (:name dory))
      (should= "Nemo" (:name nemo))
      (should= true (unidentified? dory))
      (should= false (unidentified? nemo))))

  )

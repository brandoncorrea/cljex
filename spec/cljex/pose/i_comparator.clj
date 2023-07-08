(ns cljex.pose.i-comparator
  "WIP: Creates comparators for value-returning functions."
  (:require [speclj.core :refer :all]))

(defn heavier-than? [a b]
  (#{[:hardcover :softcover]
     [:hardcover :ebook]
     [:softcover :ebook]}
   [a b]))

(describe "Comparator"
  (it "compares weights of books"
    (should-be-nil (heavier-than? :hardcover :hardcover))
    (should= [:softcover :ebook] (heavier-than? :softcover :ebook))
    (let [compare-weight (comparator heavier-than?)]
      (should= [:hardcover :hardcover :softcover :ebook]
               (sort compare-weight [:hardcover :softcover :ebook :hardcover]))))
  )

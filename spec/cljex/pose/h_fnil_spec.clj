(ns cljex.pose.h-fnil-spec
  "Rather than wrapping everything with an 'or', we can
   compose functions with default values."
  (:require [speclj.core :refer :all]))

(describe "fnil"

  (it "replaces only the first nil argument"
    (let [+nil (fnil + 10)]
      (should= 6 (+nil 1 2 3))
      (should= 13 (+nil nil 1 2))
      (should-throw NullPointerException (+nil 1 nil 2))
      (should-throw NullPointerException (+nil 1 2 nil))))

  (it "replaces only the first two nil arguments"
    (let [+nil (fnil + 10 11)]
      (should= 6 (+nil 1 2 3))
      (should= 13 (+nil nil 1 2))
      (should= 23 (+nil nil nil 2))
      (should-throw NullPointerException (+nil 1 2 nil))))

  (it "replaces only the first three nil arguments"
    (let [+nil (fnil + 10 11 12)]
      (should= 6 (+nil 1 2 3))
      (should= 13 (+nil nil 1 2))
      (should= 23 (+nil nil nil 2))
      (should= 33 (+nil nil nil nil))
      (should-throw NullPointerException (+nil 1 2 3 nil))))

  (it "is useful when you expect keys to be missing"
    (letfn [(winner [name] (str name " Wins!"))]
      (let [fnil-winner (fnil winner "Computer")
            computer    {:difficulty 3}
            charlie     {:name "Charlie"}]
        (should= "Charlie Wins!" (winner (:name charlie)))
        (should= "Charlie Wins!" (fnil-winner (:name charlie)))
        (should= " Wins!" (winner (:name computer)))
        (should= "Computer Wins!" (fnil-winner (:name computer))))))

  #_(it "cannot take more than three default arguments"
      (fnil + 1 2 3 4))
  )

(ns cljex.pose.a-constantly-spec
  "Constantly returns a predefined value... pretty basic composer"
  (:require [speclj.core :refer :all]))

(describe "constantly"

  (it "'constantly' returns the same value"
    (let [f (constantly 10)]
      (should= 10 (f nil))
      (should= 10 (f 0))
      (should= 10 (f "constantly"))
      (should= 10 (f :doesn't))
      (should= 10 (f 'care))))

  (it "takes any arity"
    (let [f (constantly :blah)]
      (should= :blah (f))
      (should= :blah (f :1))
      (should= :blah (f :1 :2))
      (should= :blah (f :1 :2 :3))
      (should= :blah (f \A \H \H \H \H \H \H \H \H \! \! \!))))

  (it "takes any value"
    (should= identity ((constantly identity)))
    (should= 'sym ((constantly 'sym)))
    (should= true ((constantly true)))
    (should= false ((constantly false)))
    (should= nil ((constantly nil)))))

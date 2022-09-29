(ns cljex.pose.f_every-pred-spec
  "If 'and' were a function... but doesn't return values (just booleans 😒)"
  (:require [speclj.core :refer :all]))

(def thing (atom 0))
(def thing+v #(swap! thing + (:v %)))
(def add-to-thing (every-pred thing+v #(some-> % :next add-to-thing)))

(describe "every-pred"

  (it "is basically a function version of 'and'"
    (let [even-pos? (every-pred pos? even?)]
      (should= true (even-pos? 4))
      (should= false (even-pos? 3))
      (should= false (even-pos? 0))
      (should= false (even-pos? -1))
      (should= false (even-pos? -2))))

  (it "short circuits on the first falsy result"
    (let [throwing-fn   (fn [& _] (throw "error"))
          even-or-throw (every-pred even? throwing-fn)]
      (should= false (even-or-throw 3))
      (should-throw (even-or-throw 4))))

  (it "always returns a boolean value (never the result of a function)"
    (let [nils        (constantly nil)
          ones        (constantly 1)
          truthy-pred (every-pred ones)
          falsy-pred  (every-pred nils)]
      (should= true (truthy-pred 5))
      (should= false (falsy-pred 5))))

  (it "can be recursive (...sorta)"
    (let [five  {:v 5}
          four  {:v 4 :next five}
          three {:v 3 :next four}
          two   {:v 2 :next three}
          one   {:v 1 :next two}]
      (reset! thing 0)
      (add-to-thing one)
      (should= 15 @thing))))

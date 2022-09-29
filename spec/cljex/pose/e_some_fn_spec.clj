(ns cljex.pose.e_some-fn-spec
  "If 'or' were a function..."
  (:require [speclj.core :refer :all]))

(def tail (some-fn #(some-> % :next tail) identity))

(describe "some-fn"

  (it "is basically a function version of 'or'"
    (let [even-or-nil? (some-fn nil? even?)]
      (should= true (even-or-nil? 4))
      (should= false (even-or-nil? 3))
      (should= true (even-or-nil? nil))))

  (it "short circuits on the first truthy result"
    (let [throwing-fn   (fn [& _] (throw "error"))
          even-or-throw (some-fn even? throwing-fn)]
      (should= true (even-or-throw 4))
      (should-throw (even-or-throw 3))))

  (it "always returns the result of the first truthy value, or the last falsy value (not necessarily a boolean)"
    (let [a-or-b (some-fn :a :b)]
      (should= 1 (a-or-b {:a 1 :b 2}))
      (should= 2 (a-or-b {:a nil :b 2}))
      (should= 2 (a-or-b {:a false :b 2}))
      (should= nil (a-or-b {:a false :b nil}))
      (should= false (a-or-b {:a nil :b false}))))

  (it "can be recursive (...sorta)"
    (let [five  {:v 5}
          four  {:v 4 :next five}
          three {:v 3 :next four}
          two   {:v 2 :next three}
          one   {:v 1 :next two}]
      (should= five (tail one))
      (should= five (tail two))
      (should= five (tail three))
      (should= five (tail four))
      (should= five (tail five)))))

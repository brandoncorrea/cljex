(ns cljex.pose.b-partial-spec
  "Creates a new function with predefined arguments.
   Arguments must be defined in order (cannot be something in the middle)"
  (:require [speclj.core :refer :all])
  (:import (clojure.lang ArityException)))

(defn hello [a b c] (str "Hello " a ", " b ", and " c))

(describe "partial"

  (it "works like the identity function when provided with one argument"
    (should= 1 (partial 1))
    (should= nil (partial nil))
    (should= "hello" (partial "hello"))
    (should= :world (partial :world))
    (should= 'bleh (partial 'bleh))
    (should= + (partial +)))

  (it "creates a new function with a pre-allocated argument"
    (let [add-ten (partial + 10)]
      (should= 11 (add-ten 1))
      (should= 30 (add-ten 20))
      (should= 0 (add-ten -10))
      (should= 10 (add-ten 0))
      (should= 10 (add-ten))))

  (it "takes any number of arguments"
    (let [hello-fred         (partial hello "Fred")
          hello-fred-ted     (partial hello "Fred" "Ted")
          hello-fred-ted-ned (partial hello "Fred" "Ted" "Ned")]
      (should= "Hello Bob, Rob, and Dob" (hello "Bob" "Rob" "Dob"))
      (should= "Hello Fred, Bob, and Rob" (hello-fred "Bob" "Rob"))
      (should= "Hello Fred, Ted, and Bob" (hello-fred-ted "Bob"))
      (should= "Hello Fred, Ted, and Ned" (hello-fred-ted-ned))))

  (it "can be a partial of another partial function"
    (let [hello-fred         (partial hello "Fred")
          hello-fred-ted     (partial hello-fred "Ted")
          hello-fred-ted-ned (partial hello-fred-ted "Ned")]
      (should= "Hello Bob, Rob, and Dob" (hello "Bob" "Rob" "Dob"))
      (should= "Hello Fred, Bob, and Rob" (hello-fred "Bob" "Rob"))
      (should= "Hello Fred, Ted, and Bob" (hello-fred-ted "Bob"))
      (should= "Hello Fred, Ted, and Ned" (hello-fred-ted-ned))))

  (it "arity is limited to that of the base function"
    (let [add       (fn [a b] (+ a b))
          add-1     (partial add 1)
          add-2-3   (partial add 2 3)
          add-5-6-7 (partial add 5 6 7)]
      (should= 3 (add-1 2))
      (should= 5 (add-2-3))
      (should-throw ArityException (add-1 2 3))
      (should-throw ArityException (add-5-6-7))))
  )

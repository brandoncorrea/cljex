(ns duce.e-attendee-transduce-spec
  "Transducers look very similar to thread-last operations,
   except instead of threading, we use function composition."
  (:require [speclj.core :refer :all]))

(def adult? (comp (partial <= 18) :age))
(def can-afford? (comp (partial <= 10) :cash))
(def people
  [{:name "Johnny" :age 12}
   {:name "Gus" :age 27 :cash 22}
   {:name "Jacob" :age 15 :cash 30}
   {:name "Trevor" :age 20 :cash 0}
   {:name "Susie" :age 31 :cash 25}])

; Filter people who are adults with cash
(def xf-attendees
  (comp
    (filter adult?) ; First, check if they're an adult
    (filter can-afford?))) ; If they're an adult, can they afford it?

; After filtering out the attendees, map over their :name
(def xf-attendee-names (comp xf-attendees (map :name)))

; After filtering out the attendees, map over their :cash
(def xf-attendee-cash (comp xf-attendees (map :cash)))

(describe "R-Rated Movies Via Transducers"
  (it "gives us the name of all the attendees"
    (should= ["Gus" "Susie"] (sequence xf-attendee-names people))
    (should= ["Gus" "Susie"] (eduction xf-attendee-names people)))

  (it "gives us information on cash totals"
    (should= [22 25] (sequence xf-attendee-cash people))
    (should= [22 25] (eduction xf-attendee-cash people))
    (should= (+ 25 22) (transduce xf-attendee-cash + people)))

  (it "returns functions rather than data"
    (should (every? fn? (map xf-attendee-names people)))
    (should (fn? (xf-attendee-names people)))))

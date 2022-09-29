(ns cljex.duce.d-attendee-thread-spec
  "If we wanted to filter out attendees for an R Rated Movie,
   here's what the code might look like using the thread-last operator."
  (:require [speclj.core :refer :all]))

(def adult? (comp (partial <= 18) :age))
(def can-afford? (comp (partial <= 10) :cash))
(def people
  [{:name "Johnny" :age 12}
   {:name "Gus" :age 27 :cash 22}
   {:name "Jacob" :age 15 :cash 30}
   {:name "Trevor" :age 20 :cash 0}
   {:name "Susie" :age 31 :cash 25}])

(defn attendees [people]
  (->> people
       (filter adult?)
       (filter can-afford?)))

(defn attendee-cash [people]
  (->> people
       attendees
       (map :cash)))

(defn attendee-names [people]
  (->> people
       attendees
       (map :name)))

(describe "R-Rated Movies Via Threading"
  (it "gives us the name of all the attendees"
    (should= ["Gus" "Susie"] (attendee-names people)))

  (it "gives us information on cash totals"
    (should= [22 25] (attendee-cash people))
    (should= (+ 25 22) (reduce + (attendee-cash people)))))

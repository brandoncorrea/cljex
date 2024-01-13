(ns cljex.log.fun-spec
  "Nothing... yet"
  (:require [clojure.core.logic :refer [== all and* conda conde conso fail fresh membero or* permuteo run run* succeed]]
            [speclj.core :refer :all]))

(describe "Logic Fun"

  (it "takes n results"
    (let [odds [1 3 5 7 9]]
      (should= (take 3 odds) (run 3 [q] (membero q odds)))
      (should= odds (run 100 [q] (membero q odds)))))

  (it "collection intersections"
    (let [coll-1 [1 2 2 3]
          coll-2 [2 2 3 4]]
      (should= [2 2 2 2 3]
               (run* [q]
                 (membero q coll-1)
                 (membero q coll-2)))))

  (it "permuteo"
    (let [vals         [1 2 3]
          permutations [[1 2 3] [2 1 3] [1 3 2] [3 1 2] [2 3 1] [3 2 1]]]
      (should= permutations
               (run* [q]
                 (fresh [a b c] ; must be of length: vals
                   (== q [a b c]) ; order doesn't matter
                   (permuteo q vals))))))

  (it "not membero"
    (should= [0 1 2 4 7 8 9]
             (run* [q]
               (membero q (range 10))
               (conda [(membero q [3 5 6]) fail]
                      [succeed]))))

  (it "fresh"
    (should= [3] (run* [q]
                   (fresh [a]
                     (membero q [1 2 3])
                     (membero a [3 4 5])
                     (== a q)))))

  (it "membero many"
    (should= [3] (run* [q]
                   (membero q [3 4 5])
                   (membero q [1 2 3]))))

  (it "or* & and*"
    (let [odds [1 3 5 7 9]
          ten  (range 1 11)]
      (should= [[1 1] [3 3] [3 5] [5 5] [7 7] [9 9]]
               (run* [odd digit]
                 (membero odd odds)
                 (membero digit ten)
                 ; same as conde
                 (or* [(and* [(== digit 5) (== odd 3)])
                       (and* [(== odd digit)])])))))

  (it "conde"
    (let [odds [1 3 5 7 9]
          ten  (range 1 11)]
      (should= [[1 1] [3 3] [3 5] [5 5] [7 7] [9 9]]
               (run* [odd digit]
                 (membero odd odds)
                 (membero digit ten)
                 (conde
                   [(== digit 5) (== odd 3)] ; x AND y
                   [(== odd digit)]))))) ; OR z

  (it "all"
    (let [odds [1 3 5 7 9]
          ten  (range 1 11)]
      (should= [[1 1] [3 3] [3 5] [5 5] [7 7] [9 9]]
               (run* [odd digit]
                 (membero odd odds)
                 (membero digit ten)
                 ; same as and*
                 (or* [(all (== digit 5) (== odd 3))
                       (all (== odd digit))])))))

  (it "conso"
    ; bind x to first and y to rest
    (should= [[1 [2 3 4]]] (run* [x y] (conso x y [1 2 3 4])))
    (should= [[1 [2 3 4] 10]
              [1 [2 3 4] 2]
              [1 [2 3 4] 3]
              [1 [2 3 4] 4]]
             (run* [x y z]
               (conso x y [1 2 3 4])
               (conde [(membero z y)]
                      [(== z 10)]))))

  (it "conda"
    (let [things ["foo" "bar" "baz" "buzz" "bleh"]]
      (should= (keep (fn [thing]
                       (cond
                         (#{"baz" "buzz"} thing) [thing :baz-buzz]
                         (= "foo" thing) [thing :foo]
                         (= "bleh" thing) [thing :bleh])) things)
               (run* [thing n]
                 (membero thing things)
                 (conda
                   [(membero thing ["baz" "buzz"]) (== n :baz-buzz)]
                   [(== thing "foo") (== n :foo)]
                   [(== thing "bleh") (== n :bleh)])))))

  (it "membero q"
    ; q MUST be 7
    (should= [7] (run* [q] (membero 7 [1 3 8 q])))
    ; q MAY be 7, or anything else under the sun
    (should= ['_0 7] (run* [q] (membero 7 [1 3 8 7 q]))))
  )

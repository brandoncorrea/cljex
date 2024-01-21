(ns cljex.log.fun-spec
  "Nothing... yet"
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer [== all and* conda conde conso distincto fail fresh membero or* permuteo run run* succeed]]
            [clojure.core.logic.fd :as fd]
            [clojure.math.combinatorics :as combo]
            [speclj.core :refer :all]))

(defmacro elapsed-time
  "Evaluates the body and returns the elapsed time in milliseconds."
  [& body]
  `(let [start# (System/nanoTime)]
     (do ~@body)
     (/ (double (- (System/nanoTime) start#)) 1000000.0)))

(defn reduce-times
  "Reduces on a function n times. f is a single-arity function,
   which accepts the result of its last invocation."
  ([f n] (reduce-times f nil n))
  ([f init n] (reduce (fn [acc _] (f acc)) init (range n))))

(defmacro bench-n
  "Test how long a body will take to execute.
   This will only `(do body)`. If you need to realize
   all results in a sequence, wrap your body in a `doall`.
   Body is executed n times."
  [n & body]
  `(let [n# ~n]
     (when (pos? n#)
       (let [timings# (repeatedly n# #(elapsed-time ~@body))
             total#   (apply + timings#)]
         {:min   (apply min timings#)
          :max   (apply max timings#)
          :total total#
          :avg   (/ total# n#)}))))

(defmacro bench
  "Same as (bench-n 10 ...)"
  [& body] `(bench-n 10 ~@body))

(defmacro sum
  ([x y binding] `(fd/+ ~x ~y ~binding))
  ([x y z binding]
   `(fresh [s#]
      (fd/+ ~x ~y s#)
      (fd/+ s# ~z ~binding)))
  ([w x y z binding]
   `(fresh [s1# s2#]
      (fd/+ ~w ~x s1#)
      (fd/+ s1# ~y s2#)
      (fd/+ s2# ~z ~binding))))

(defmacro sum== [comp & vars]
  `(fresh [s#]
     (sum ~@vars s#)
     (== s# ~comp)))

(defn magic-constant
  ([n] (magic-constant 1 n))
  ([start n]
   (/ (* n
         (dec
           (+ (* 2 start)
              (* n n))))
      2)))

(defn magic-3x3-squares [lower-bound upper-bound]
  (let [median         (- upper-bound (/ (- upper-bound lower-bound) 2))
        magic-constant (magic-constant 3)]
    (run* [q]
      (fresh [a b c
              d e f
              g h i]

        (distincto [a b c d e f g h i])
        (fd/in a b c d e f g h i (fd/interval lower-bound upper-bound))

        ;; Odd Squares will always have the median value in its center
        (== e median)
        (fresh [row-sum]
          ;; All rows, columns, and main diagonals will sum to the magic constant.
          (== row-sum magic-constant)
          (sum a b c row-sum)
          (sum== row-sum d e f)
          (sum== row-sum g h i)
          (sum== row-sum a d g)
          (sum== row-sum b e h)
          (sum== row-sum c f i)
          (sum== row-sum a e i)
          (sum== row-sum c e g))

        (== q [a b c d e f g h i])))))

(defn magic-4x4-squares [limit lower-bound upper-bound]
  (let [magic-constant (magic-constant 3)]
    (run limit [q]
      (fresh [a b c d
              e f g h
              i j k l
              m n o p]
        (distincto [a b c d e f g h i j k l m n o p])
        (fd/in a b c d e f g h i j k l m n o p (fd/interval lower-bound upper-bound))

        (fresh [row-sum]
          ;; All rows, columns, and main diagonals will sum to the magic constant.
          (== row-sum magic-constant)
          (sum a b c d row-sum)
          (sum== row-sum e f g h)
          (sum== row-sum i j k l)
          (sum== row-sum m n o p)
          (sum== row-sum a e i m)
          (sum== row-sum b f j n)
          (sum== row-sum c g k o)
          (sum== row-sum d h l p)
          (sum== row-sum a f k p)
          (sum== row-sum m j g d))

        (== q [a b c d e f g h i j k l m n o p])))))

(defn indices-for-square [n]
  (let [n-1           (dec n)
        n+1           (inc n)
        range-n       (vec (range n))
        col-indices   (map (fn [c] (map #(+ (* n %) c) range-n)) range-n)
        row-indices   (partition n (range (* n n)))
        diag1-indices (map #(* % n+1) range-n)
        diag2-indices (map #(* % n-1) (range 1 n+1))]
    (mapv vec (concat col-indices row-indices [diag1-indices diag2-indices]))))

(defn generate-magic-squares
  ([n] (generate-magic-squares n 1))
  ([n start]
   (let [;; All rows, columns, and main diagonals will sum to the magic constant.
         magic-constant (magic-constant start n)
         magic-indices? (fn [coll indices] (= magic-constant (reduce #(+ %1 (nth coll %2)) 0 indices)))
         indices        (indices-for-square n)
         upper-bound    (+ start (* n n))
         interval       (range start upper-bound)
         median-index   (int (/ (* n n) 2))
         median-val     (nth interval median-index)]
     (->> (combo/permutations interval)
          (filter
            (cond->> #(every? (partial magic-indices? %) indices)
                     ;; Odd Squares will always have the median value in its center
                     (odd? n)
                     (every-pred #(= median-val (nth % median-index)))))))))

(describe "Logic Fun"

  (it "filtering"
    (let [vals (shuffle (range 1 101))
          odds (shuffle (filter odd? vals))]
      (should= (set (filter (fn [m] (some #(= % m) odds)) vals))
               (set
                 (run* [q]
                   (membero q vals)
                   (membero q odds))))))

  (it "magic squares"
    (should= 15 (magic-constant 3))
    (should= 34 (magic-constant 4))
    (should= 34 (magic-constant 1 4))
    (should= 70 (magic-constant 10 4))
    ;(should= 1 (bench-n 10 (doall (generate-magic-squares 3))))
    (should= (set (generate-magic-squares 3))
             (set (magic-3x3-squares 1 9)))
    ;; WLL NOT TERMINATE
    #_(should= (set (generate-magic-squares 4))
               (set (magic-4x4-squares 1 1 16))))

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

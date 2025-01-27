(ns user
  (:require [clojure.test :refer [testing is run-tests deftest]]
            [clojure.string]))

(deftest problem-105s
  (testing "105, Identify keys and values"
    ;; Given a sequence of keywords and numbers, create a map such
    ;;  that each key in the map is a keyword, and the value is a 
    ;;  sequence of all the numbers (if any) between it and the next 
    ;;  keyword in the sequence.

    ;; if is the other way, map -> coll
    ;; (let [f (fn [mp]
    ;;  (first (for [x mp] 
    ;;    (if (and (coll? x) (not (nil? x))) (flatten x)
    ;;       x)))
    ;; Short version
    ;;  (partition-by keyword?)
    ;;  (mapcat (fn [[k :as v]] (if (keyword? k) (interpose [] v) [v])))
    ;;  (apply hash-map))
    ;; Readable version
    (let [f #(reduce 
      (fn [m v]
        (if (keyword? v)
          (assoc m v [])
          (let [k (-> m keys sort last)
                s (get m k)]
            (assoc m k (conj s v)))))
      {}
      %)]
      (is (= {} (f [])))
      (is (= {:a [1]} (f [:a 1])))
      (is (= {:a [1] :b [2]} (f [:a 1, :b 2])))
      (is (= {:a [1 2 3] :b [] :c [4]} (f [:a 1 2 3 :b :c 4])))
    )
  )
  (testing "106, Hard - Number Maze"
    ;; Given the start and end point #s, find a path between the two using only
    ;;  three possible operations:
    ;; 1. double
    ;; 2. halve (odd numbers cannot be halved)
    ;; 3. add 2 Find the shortest path through the “maze”.
    ;; Because there are multiple shortest paths, you must
    ;;  return the length of the shortest path, not the path itself.

    (let [f (fn [x y]
      (loop [r [x] s 1]
        (if (some #(= y %) r) 
          s 
          (recur ((fn [c] (mapcat #((juxt * / +) % 2) c)) r) (inc s)))
      ))]
      (is (= 1 (f 1 1)))
      (is (= 3 (f 3 12)))
      (is (= 3 (f 12 3)))
      (is (= 3 (f 5 9)))
      (is (= 9 (f 9 2)))
      (is (= 5 (f 9 12)))
    )
  )
  (testing "107, Simple Closures"
    ;; Given a + integer n, return a function (f x) which computes xn.
    ;; Observe that the effect of this is to preserve the value of n 
    ;;  for use outside the scope in which it is defined.
    (let [f (fn [a] (fn [b] (int (Math/pow b a))))
      ]
      (is (= 256 ((f 2) 16), ((f 8) 2)))
      (is (= [1 8 27 64] (map (f 3) [1 2 3 4])))
      (is (= [1 2 4 8 16] (map #((f %) 2) [0 1 2 3 4])))
    )
  )
  (testing "108, Lazy Searching"
    ;; Given any number of sequences, each sorted from smallest to largest,
    ;; find the smallest single number which appears in all of the sequences.
    ;; The sequences may be infinite, so be careful to search lazily.
    (let [f (fn [& p]
      (loop [p p]
        (let [x (apply min (map first p))]
          (if (apply = (map first p)) x
            (recur (map #(if (= x (first %)) (rest %) %) p))))))
      ]
      (is (= 3 (f [3 4 5])))
      (is (= 4 (f [1 2 3 4 5 6 7] [0.5 3/2 4 19])))
      (is (= 64 (f (map #(* % % %) (range))
                   (filter #(zero? (bit-and % (dec %))) (range))
                   (iterate inc 20))))
      (is (= 7 (f (range) (range 0 100 7/6) [2 3 5 7 11 13])))
    )
  )
  ;; No 109
)

(run-tests)
(ns user
  (:require [clojure.test :refer [testing is run-tests deftest]]
            [clojure.string :as cljstr]))

(deftest sequences-2
  (testing "28, Flatten a Sequence, w/o flatten"
    (let [f
      (fn [x] (filter (complement sequential?)
          (rest (tree-seq sequential? seq x))))]
      (is (= (f '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6)))
      (is (= (f ["a" ["b"] "c"]) '("a" "b" "c")))
      (is (= (f '((((:a))))) '(:a)))
    )
  )
  (testing "29, Filter Cap-chars"
    (let [f #(cljstr/replace % #"[^A-Z]" "")]
      (is (= (f "HeLlO, WoRlD!") "HLOWRD"))
      (is (empty? (f "nothing")))
      (is (= (f "$#A(*&987Zf") "AZ"))
    )
  )
  (testing "30, Remove consecutive duplicates from a sequence"
    (let [f #(reduce
        (fn [acc c]
          (if (not= c (last acc)) (conj acc c) acc)
        ) [] %)]
      (is (= (apply str (f "Leeeeeerrroyyy")) "Leroy"))
      (is (= (f [1 1 2 3 3 2 2 3]) '(1 2 3 2 3)))
      (is (= (f [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2])))
    )
  )
  (testing "31, Pack consecutive duplicates into sub-lists"
    (let [f #(reduce
        (fn [x y] 
          (if (= y (first (last x)))
            (conj (pop x) (conj (last x) y)) 
            (conj x (conj [] y)))
        ) [] %)]
      (is (= (f [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3))))
      (is (= (f [:a :a :b :b :c]) '((:a :a) (:b :b) (:c))))
      (is (= (f [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4]))))
    )
  )
  (testing "32, Duplicate each element of a sequence"
    (let [f #(interleave % %)]
      (is (= (f [1 2 3]) '(1 1 2 2 3 3)))
      (is (= (f [:a :a :b :b]) '(:a :a :a :a :b :b :b :b)))
      (is (= (f [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])))
      (is (= (f [44 33]) [44 44 33 33]))
    )
  )
  (testing "33, Replicate each element of a sequence a variable # of times"
    (let [f (fn [col n] (mapcat #(repeat n %) col))]
      (is (= (f [1 2 3] 2) '(1 1 2 2 3 3)))
      (is (= (f [:a :b] 4) '(:a :a :a :a :b :b :b :b)))
      (is (= (f [4 5 6] 1) '(4 5 6)))
      (is (= (f [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4])))
      (is (= (f [44 33] 2) [44 44 33 33]))
    )
  )
  (testing "34, Create a list of all integers within a range, w/o range"
    (let [f #(take
        (- %2 %1)
        (iterate inc %1))]
      (is (= (f 1 4) '(1 2 3)))
      (is (= (f -2 2) '(-2 -1 0 1)))
      (is (= (f 5 8) '(5 6 7)))
    )
  )
  (testing "37, Regex"
    (is (= "ABC" (apply str (re-seq #"[A-Z]+" "bA1B3Ce "))))
  )
  (testing "38, Return max value in a sequence, w/o max, max-key"
    (let [f (fn [& args] (reduce #(if (> %1 %2) %1 %2) args))]
      (is (= (f 1 8 3 4) 8))
      (is (= (f 30 20) 30))
      (is (= (f 45 67 11) 67))
    )
  )
)

(run-tests)
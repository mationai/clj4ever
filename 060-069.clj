(ns user
  (:require [clojure.test :refer [testing is run-tests deftest]]))

(deftest sequences-5
  (testing "60, Med - Sequence Reductions, w/o reductions"  
  ;; function that behaves like reduce, but returns each intermediate
  ;;  value of the reduction. It must accept either two or three
  ;;  arguments. Return sequence must be lazy. Using reductions:
    ;; (let [f (fn 
    ;;       ([f col] (reductions f col))
    ;;       ([f init col] (reductions f init col)))]
    (let [f (fn r
      ([f col] (r f (first col) (rest col)))
      ([f i col]
        (cons i (lazy-seq
              (when-first [a col]
                (r f (f i a) (rest col)))))))]
      ;; Eg. with + [1 2 3 4]:
      ;; Initial call: (r + [1 2 3 4])
      ;; -> (r + 1 [2 3 4])    ; splits into first and rest

      ;; Step 1: (cons 1 ...)  ; adds 1 to result
      ;; Next: a=2, (+ 1 2)=3  ; -> (r + 3 [3 4])

      ;; Step 2: (cons 3 ...)  ; adds 3 to result
      ;; Next: a=3, (+ 3 3)=6  ; -> (r + 6 [4])

      ;; Step 3: (cons 6 ...)  ; adds 6 to result
      ;; Next: a=4, (+ 6 4)=10 ; -> (r + 10 [])

      ;; Step 4: (cons 10 nil) ; adds final 10
      ;; Result: (1 3 6 10)    ; running sum at each step

      (is (= (take 5 (f + (range))) [0 1 3 6 10]))
      (is (= (f conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]]))
      (is (= (last (f * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120))
    )
  )
  (testing "61, Map Construction, w/o zipmap"  
    ;; Function that takes a vector of keys and a vector of values
    ;;  and constructs a map from them.
    ;; (let [f #(zipmap %1 %2)] ;; works
    (let [f #(apply hash-map (mapcat list % %2))]
      (is (= (f [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3}))
      (is (= (f [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"}))
      (is (= (f [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"}))
    )
  )
  (testing "62, Implement Iteration, w/o iterate"  
    ;; Given a side-effect free function f and an initial value x
    ;;  create function that returns an infinite lazy sequence of
    ;;  x, (f x), (f (f x)), (f (f (f x))), etc.
    ;; (let [f #(iterate % %2)] ;; works
    (let [f (fn r [f x] (cons x (lazy-seq (r f (f x)))))]
      (is (= (take 5 (f #(* 2 %) 1)) [1 2 4 8 16]))
      (is (= (take 100 (f inc 0)) (take 100 (range))))
      (is (= (take 9 (f #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3]))))
    )
  )
  (testing "63, Group a Sequence, w/o group-by"  
    ;; Given a function f and a sequence s, create a function that
    ;;  returns a map. The keys should be the values of f applied
    ;;  to each item in s. The value at each key should be a vector 
    ;;  of corresponding items in the order they appear in s.
    ;; (let [f #(group-by % %2)] ;; works
    (let [f #(apply merge-with into
      (for [x %2] {(% x) [x]}))] ;; for x in seq, {f(x) [x]}
      ;; then merge all maps using into
      ;; apply spreads the seq of maps as args to merge-with
      (is (= (f #(> % 5) #{1 3 6 8}) {false [1 3], true [6 8]}))
      (is (= (f #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
        {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]}))
      (is (= (f count [[1] [1 2] [3] [1 2 3] [2 3]])
        {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]}))
    )
  )
  (testing "64, Reduce"
    (let [f (fn [& args] (apply + args))]
      (is (= 15 (reduce f [1 2 3 4 5])))
      (is (=  0 (reduce f [])))
      (is (=  6 (reduce f 1 [2 3])))
    )
  )
  (testing "65, Black Box Testing"  
    (let [f #(condp = (empty %)
      {} :map
      #{} :set
      (if (ifn? %) :vector :list))]
      (is (= :map (f {:a 1, :b 2})))
      (is (= :list (f (range (rand-int 20)))))
      (is (= :vector (f [1 2 3 4 5 6])))
      (is (= :set (f #{10 (rand-int 5)})))
      (is (= [:map :set :vector :list] (map f [{} #{} [] ()])))
    )
  )
  (testing "66, GCD - Greatest Common Divisor"  
    (let [f #(if (zero? %2) 
      % 
      (recur %2 (rem % %2)))]
      (is (= (f 2 4) 2))
      (is (= (f 10 5) 5)) ;; (recur 5 (rem 10 5)=0) -> 5
      (is (= (f 5 7) 1))
      (is (= (f 1023 858) 33))
      ;; (rem 1023 858) = 165 -> (recur 858 165)  
      ;; (rem  858 165) =  33 -> (recur 165  33)
      ;; (rem  165  33) =   0
    )
  )
  (testing "67, First n prime numbers"  
    (let [f (fn [n]
      (take n
        (filter #(.isProbablePrime (BigInteger/valueOf %) 10)
        ;; .isProbablePrime 10 tests if it's probably prime
        ;; 10 is the certainty parameter (higher = more certain)
          (range))))]
      (is (= (f 2) [2 3]))
      (is (= (f 5) [2 3 5 7 11]))
      (is (= (last (f 100)) 541))
    )
  )
  (testing "68, Recur"  
    (is (=
      [7 6 5 4 3]
      (loop [x 5 result []]
        (if (> x 0)
          (recur (dec x) (conj result (+ 2 x)))
          ;; ^ (loop [4 [7]]) ... to build result to [7 6 5 4 3] 
          result)))) ;; returns [7 6 5 4 3] when x=0 is passed 
  )
  (testing "69, Merge with, w/o merge-with"  
    ;; Function that takes a function f and a variable number of maps.
    ;;  Function should return a map that consists of the rest of the maps
    ;;  conj-ed onto the first. If a key occurs in more than one map, the
    ;;  mapping(s) from the latter (left-to-right) should be combined with
    ;;  the mapping in the result by calling (f val-in-result val-in-latter)
    ;; (let [f #(apply merge-with % %&)] ;; works
    (let [f #(into {} 
      (for [[k s] (group-by key (apply concat %&))]
            [k (reduce % (vals s))]))]
      (is (= (f * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
        {:a 4, :b 6, :c 20}))
      (is (= (f - {1 10, 2 20} {1 3, 2 10, 3 15})
        {1 7, 2 10, 3 15}))
      (is (= (f concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
        {:a [3 4 5], :b [6 7], :c [8 9]}))
    )
  )
)

(run-tests)
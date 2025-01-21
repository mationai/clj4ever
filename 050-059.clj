(ns user
  (:require [clojure.test :refer [testing is run-tests deftest]]
            [clojure.string :as cljstr]))

(deftest sequences-4
  (testing "50, Split a sequence by type"  
    (let [f #(vals (group-by type %))]
      (is (= (set (f [1 :a "apple" 2 :b "banana"])) #{[1 2] [:a :b] ["apple" "banana"]}))
      (is (= (set (f [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]}))
      (is (= (set (f [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]}))
    )
  )
  (testing "51, Advanced Destructuring"  
    (let [expected [1 2 [3 4 5] [1 2 3 4 5]]]
      (is (=
        expected
        (let [[a b & c ;; & c - c gets the rest as a vector
          :as d ;; :as d - d gets the whole input list
        ] [1 2 3 4 5]] [a b c d]))
      )
    )
  )
  (testing "52, More destructuring (and range function)" 
    (is (= [2 4] (let [[a b c d e f g] (range)] [c e]))
    ) ;; range returns 0 to n where n is max needed to destruct [a..g]
  )
  (testing "53, HARD - get longest increasing subsequence" 
    (let [f (fn [col]
      (apply (partial max-key count) [] (reverse
        ;; max-key returns last if tie, reverse for first
        (for [win (range 2 (inc (count col))) 
              nums (partition win 1 col) ;; get windows of col
              :when (apply < nums)] ;; filter n's in nums in increasing order
          nums))))]
      (is (= (f [1 0 1 2 3 0 4 5]) [0 1 2 3]))
      (is (= (f [5 6 1 3 2 7]) [5 6]))
      (is (= (f [2 3 3 4 5]) [3 4 5]))
      (is (= (f [7 6 5 4]) []))
    )
  )
  (testing "54, Med - Partition a sequence, w/o partition*" 
    ;; Return a seq of lists of x items each.
    ;; List of less than x items is not returned.
    ;; (let [f #(partition % %2)] ;; works
    (let [f (fn fun [n col]
      (if (< (count col) n)
        '()
        (cons (take n col) (fun n (drop n col)))))]
      (is (= (f 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8))))
      (is (= (f 2 (range 8)) '((0 1) (2 3) (4 5) (6 7))))
      (is (= (f 3 (range 8)) '((0 1 2) (3 4 5))))
    )
  )
  (testing "55`, Return count map of sequence, w/o frequencies"  
    ;; (let [f #(frequencies %)] ;; works
    (let [f (fn [col]
      (reduce #(assoc % %2 (inc (% %2 0))) {} col))]
      ;; Step 1:
      ;;  (% %2 0) -> ({} 1(item-0) 0) -> 1 in {} ? no -> 0
      ;;  (inc 0) -> 1
      ;;  (assoc {} 1 1) -> {1 1}
      (is (= (f [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1}))
      (is (= (f [:b :a :b :a :b]) {:a 2 :b 3}))
      (is (= (f [[1 2] [1 3] [1 3]]) {[1 2] 1 [1 3] 2}))
    )
  )
  (testing "56, Remove dups in sequence, w/o distinct"  
    (let [f (fn [col]
      ;; (keys (reduce #(assoc % %2 1) {} col)))]
      ;;  works, but returns out of order for range 50 eg.
      (reduce #(if (contains? (set %) %2) % (conj % %2)) [] col))]
      (is (= (f [1 1 2 3 2 1 1]) [1 2 3]))
      (is (= (f [:a :a :b :b :c :c]) [:a :b :c]))
      (is (= (f [[2 4] [1 2] [1 3] [1 3]]) [[2 4] [1 2] [1 3]]))
      (is (= (sort (f (range 50))) (range 50)))
    )
  )
  (testing "57, Simple Recursion"  
    (= '(5 4 3 2 1)
      ((fn foo [x] (when (> x 0) (conj (foo (dec x)) x))) 5))
  )
  (testing "58, Function Composition, w/o comp"  
    ;; function take a set of functions and
    ;;  return a function applies them from right-to-left.
    ;; (let [f (fn [& fs] (apply comp fs))] ;; works
    (let [f (fn [& fs]
      (reduce (fn [f g] #(f (apply g %&))) fs))]
      (is (= [3 2 1] ((f rest reverse) [1 2 3 4]))) ;; reverse, rest
      (is (= 5 ((f (partial + 3) second) [1 2 3 4]))) ;; 2nd, + 3
      (is (= true ((f zero? #(mod % 8) +) 3 5 7 9)))
      ;; sum -> 24, mod 8 -> 0, is zero
      (is (= "HELLO" ((f #(.toUpperCase %) #(apply str %) take) 5 "hello world")))
      ;; 5 first chars, join (apply str), to upper -> "HELLO" 
    )
  )
  (testing "59, Juxtaposition, w/o juxt"  
  ;; Function takes a set of functions and
  ;;  return a function that takes a variable number of arguments 
  ;;  and returns a sequence containing the result of applying
  ;;  each function left-to-right to the argument list
    ;; (let [f (fn [& fs] (apply juxt fs))] ;; works
    (let [f (fn [& fs]
      (fn [& args] (map #(apply % args) fs)))]
      (is (= [21 6 1] ((f + max min) 2 3 5 1 6 4))) ;; 21=sum, 6=max, 1=min
      (is (= ["HELLO" 5] ((f #(.toUpperCase %) count) "hello")))
      (is (= [2 6 4] ((f :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10})))
    )
  )
)

(run-tests)
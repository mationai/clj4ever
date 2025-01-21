(ns user
  (:require [clojure.test :refer [testing is run-tests deftest]]
))

(deftest sequences-3
  (testing "39, Interleave two sequences, w/o interleave"
    ;; (let [f #(interleave % %2)] ;; works
    (let [f #(mapcat list % %2)] ;; mapcat list = interleave,
      ;; by mapping list to each element pair, then flatten w/ cat
      (is (= (f [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c)))
      (is (= (f [1 2] [3 4 5 6]) '(1 3 2 4)))
      (is (= (f [1 2 3 4] [5]) [1 5]))
      (is (= (f [30 20] [25 15]) [30 25 20 15]))
    )
  )
  (testing "40, Interpose a sequence, w/o interpose"
    ;; inject a separator instead of items in a list
    ;; (let [f #(interpose % %2)] ;; works
    (let [f (fn [n col] (drop-last (mapcat #(list % n) col)))]
      ;; list % n = (% n), cat concats all pairs, drop-last drops separator
      (is (= (f 0 [1 2 3]) [1 0 2 0 3]))
      (is (= (apply str (f ", " ["1" "2" "3"])) "1, 2, 3"))
      (is (= (f :z [:a :b :c :d]) [:a :z :b :z :c :z :d]))
    )
  )
  (testing "41, Drop every Nth element from a sequence"
    (let [f (fn [col n] ;; n = nth step
      (flatten (partition-all (dec n) n col)))]
      ;; partition-all partitions last even if last is not full
      ;; partition-all size step col, w/ size = n-1 = drop last
      (is (= (f [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8]))
      (is (= (f [:a :b :c :d :e :f] 2) [:a :c :e]))
      (is (= (f [1 2 3 4 5 6] 4) [1 2 3 5 6]))
    )
  )
  (testing "42, Return the factorial of n"
    (let [f #(apply * (range 1 (inc %)))]
      (is (= (f 1) 1))
      (is (= (f 3) 6))
      (is (= (f 5) 120))
      (is (= (f 8) 40320))
    )
  )
  (testing "43, Un-interleave a sequence to n sub-sequences"
    (let [f (fn [col n]
      (apply map list (partition n col)))]
      ;; For eg. [1 2 3 4 5 6], n=2:
      ;; partition n col = split into n lists eg. [(1 2) (3 4) (5 6)] 
      ;; map list = pair each item in lists eg. (1 3 5) (2 4 6)
      ;; but map expects individual items as args, so apply is needed
      (is (= (f [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6))))
      (is (= (f (range 9) 3) '((0 3 6) (1 4 7) (2 5 8))))
      (is (= (f (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))) 
    )
  )
  (testing "44, Rotate a sequence, neg rotaties in other direction"
    (let [f #(let [n (mod % (count %2))]
      (concat (drop n %2) (take n %2)))]
      ;; (take n %2) pushes the first n items to the end, neg rotation is reverse
      ;; (drop n %2) moves the first n items
      ;; where n = mod % (len of %2)
      (is (= (f 2 [1 2 3 4 5]) '(3 4 5 1 2)))
      (is (= (f -2 [1 2 3 4 5]) '(4 5 1 2 3)))
      (is (= (f 6 [1 2 3 4 5]) '(2 3 4 5 1)))
      (is (= (f 1 '(:a :b :c)) '(:b :c :a)))
      (is (= (f -4 '(:a :b :c)) '(:c :a :b)))
    )
  )
  (testing "45, Sequence generating functions"
    (is (= '(1 4 7 10 13) (take 5 (iterate #(+ 3 %) 1))))
  )
  (testing "46, Flip the order of args of an input function"
    (let [f (fn [f] #(f %2 %))]
      (is (= 3 ((f nth) 2 [1 2 3 4 5])))
      (is (= true ((f >) 7 8)))
      (is (= 4 ((f quot) 2 8)))
      (is (= [1 2 3] ((f take) [1 2 3 4 5] 3)))
    )
  )
  (testing "47, The contains function"
    (is (contains? #{4 5 6} 4))
    (is (contains? [1 1 1 1 1] 4)) ;; 4 exists as an index
    (is (contains? {4 :a 2 :b} 4)) ;; contains checks keys
    (is (not (contains? [1 2 4] 4))) ;; 4 NOT exists as an index
  )
  (testing "48, some function - returns first truthy match in %2"
    (is (= 6 (some #{2 7 6} [5 6 7 8])))
    (is (= 6 (some #(when (even? %) %) [5 6 7 8])))
  )
  (testing "49, Split a sequence into two parts, w/o split-at"
    ;; (let [f #(split-at % %2)] ;; works
    (let [f #(list (take % %2) (drop % %2))]
      (is (= (f 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]]))
      (is (= (f 1 [:a :b :c :d]) [[:a] [:b :c :d]]))
      (is (= (f 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]]))
    )
  )
)

(run-tests)
(ns user
  (:require [clojure.test :refer [testing is run-tests deftest]]
            [clojure.set :as cljset]))

(deftest lists-vectors
  (testing "4, Lists" 
    (is (=  (list :a :b :c) '(:a :b :c)))
  )
  (testing "5, conj on lists" 
    ;; conj adds %2+ to the front of %1 for lists (linked lists) 
    (let [x '(1 2 3 4)]
      (is (= x (conj '(2 3 4) 1)))
      (is (= x (conj '(3 4) 2 1)))
    )
  )
  (testing "6, Vectors" 
    (is (= [:a :b :c]
           (list :a :b :c)
           (vec '(:a :b :c))
           (vector :a :b :c)))
  )
  (testing "7, conj on vectors" 
    ;; conj adds %2+ to the back of %1 for vectors (arrays)
    (let [x [1 2 3 4]]
      (is (= x (conj [1 2 3] 4)))
      (is (= x (conj [1 2] 3 4)))
    )
  )
)

(deftest sets-maps
  (testing "8, Sets" 
    (let [x #{:a :b :c :d}]
      (is (= x (set '(:a :a :b :c :c :c :c :d :d))))
      (is (= x (cljset/union #{:a :b :c} #{:b :c :d})))
    )
  )
  (testing "9, conj on sets" 
    (is (= #{1 2 3 4} (conj #{1 4 3} 2)))
  )
  (testing "10, Maps" 
    (is (= 20 ((hash-map :a 10, :b 20, :c 30) :b)))
    (is (= 20 (:b {:a 10, :b 20, :c 30})))
  )
  (testing "11, conj on maps" 
    (is (= {:a 1, :b 2, :c 3} (conj {:a 1} {:b 2} [:c 3])))
    (is (= {:a 1, :b 2, :c 3} (conj {:a 1} [:b 2] [:c 3])))
  )
)

(deftest seqs-rest
  (testing "12, Sequences" 
    (is (= 3 (first '(3 2 1))))
    (is (= 3 (second '(2 3 4))))
    (is (= 3 (last (list 1 2 3))))
  )
  (testing "13, rest" 
    (is (= [20 30 40] (rest [10 20 30 40])))
  )
)

(deftest functions
  (testing "14, Functions" 
    (is (= 8 ((fn add-five [x] (+ x 5)) 3)))
    (is (= 8 ((fn [x] (+ x 5)) 3)))
    (is (= 8 (#(+ % 5) 3)))
    (is (= 8 ((partial + 5) 3)))
  )
  (testing "16, #(str a b c) = abc" 
    (let [f #(str "Hello, " % "!")]
      (is (= (f "Dave") "Hello, Dave!"))
    )
  )
)

(deftest map-filter 
  (testing "17, map f sequence"
    (is (= [6 7 8] (map #(+ % 5) '(1 2 3))))
  )
  (testing "18, filter"
    (is (= [6 7] (filter #(> % 5) '(3 4 5 6 7))))
  )
)
(run-tests)
(ns user
  (:require [clojure.test :refer [testing is run-tests deftest]]
            [clojure.set :as cljset]))

(deftest sequences
  (testing "19, Last Element, w/o last" 
    (let [f #(first (reverse %))]
      (is (= (f [1 2 3 4 5]) 5))
      (is (= (f '(5 4 3)) 3))
      (is (= (f ["b" "c" "d"]) "d"))
    )
  )
  (testing "20, 2nd to last Element" 
    (let [f #(second (reverse %))]
      (is (= (f (list 1 2 3 4 5)) 4))
      (is (= (f ["a" "b" "c"]) "b"))
      (is (= (f [[1 2] [3 4]]) [1 2]))
    )
  )
  (testing "21, Nth Element, no nth" 
    (let [f (fn [a b] ((vec a) b))]
      (is (= (f '(4 5 6 7) 2) 6))
      (is (= (f [:a :b :c] 0) :a))
      (is (= (f [1 2 3 4] 1) 2))
      (is (= (f '([1 2] [3 4] [5 6]) 2) [5 6]))
    )
  )
  (testing "22, Count # of Elements, no count" 
    (let [f #(reduce (fn [acc _] (inc acc)) 0 %)]
      (is (= (f '(1 2 3 3 1)) 5))
      (is (= (f "Hello World") 11))
      (is (= (f [[1 2] [3 4] [5 6]]) 3))
      (is (= (f '(13)) 1))
      (is (= (f '(:a :b :c)) 3))
    )
  )
  (testing "23, Reverse a sequence, no reverse" 
    (let [f #(reduce conj '() (seq %))]
      (is (= (f [1 2 3 4 5]) [5 4 3 2 1]))
      (is (= (f (sorted-set 5 7 2 7)) '(7 5 2)))
      (is (= (f [[1 2] [3 4] [5 6]]) [[5 6][3 4][1 2]]))
    )
  )
  (testing "24, Sum a sequence of numbers" 
    (let [f #(reduce + 0 %)]
      (is (= (f [1 2 3]) 6))
      (is (= (f (list 0 -2 5 5)) 8))
      (is (= (f #{4 2 1}) 7))
      (is (= (f '(0 0 -1)) -1))
      (is (= (f '(1 10 3)) 14))
    )
  )
  (testing "25, Find the odd numbers in a sequence" 
    (let [f (fn [a] (filter #(= (mod % 2) 1) a))]
      (is (= (f #{1 2 3 4 5}) '(1 3 5)))
      (is (= (f [4 2 1 6]) '(1)))
      (is (= (f [2 2 4 6]) '()))
      (is (= (f [1 1 1 3]) '(1 1 1 3)))
    )
  )
  (testing "26, Find the first x fibonacci numbers" 
    (let [f
      #(take %1
        ((fn r [a b]
          (lazy-seq (cons a (r b (+ a b))))
        ) 1 1))]
      (is (= (f 3) '(1 1 2)))
      (is (= (f 6) '(1 1 2 3 5 8)))
      (is (= (f 8) '(1 1 2 3 5 8 13 21)))
    )
  )
  (testing "27, Palindrome Function, return true if input is a palindrome" 
    (let [f (fn [x] (= (reverse x) (seq x)))]
      (is (false? (f '(1 2 3 4 5))))
      (is (true? (f "racecar")))
      (is (true? (f [:foo :bar :foo])))
      (is (true? (f '(1 1 3 3 1 1))))
      (is (false? (f '(:a :b :c))))
    )
  )
)
(run-tests)
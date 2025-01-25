(ns user
  (:require [clojure.test :refer [testing is run-tests deftest]]))

(deftest problem-95s
  (testing "95, If input is a (value, left, right) tree construct or not"
    (let [f (fn tree? [s]
      (if (and
            (coll? s)
            (= 3 (count s))
            (not (coll? (first s))))
        (let [[_ L R] s]
          (and (or (nil? L) (tree? L))
               (or (nil? R) (tree? R))))
        false))
      ]
      (is (= (f '(:a (:b nil nil) nil))
        true))
      (is (= (f '(:a (:b nil nil)))
        false))
      (is (= (f [1 nil [2 [3 nil nil] [4 nil nil]]])
        true))
      (is (= (f [1 [2 nil nil] [3 nil nil] [4 nil nil]])
        false))
      (is (= (f [1 [2 [3 [4 nil nil] nil] nil] nil])
        true))
      (is (= (f [1 [2 [3 [4 false nil] nil] nil] nil])
        false))
      (is (= (f '(:a nil ()))
        false)) 
    )
  )
  (testing "96, Symmetric Tree"
    ;; A binary tree is symmetric if its L is a mirror image of its R
    (let [f (fn [[_ L R]]
      (= L ((fn rf [[n L R]]
              (cond n [n (rf R) (rf L)]))
         R)))]
      (is (= (f '(:a (:b nil nil) (:b nil nil))) true))
      (is (= (f '(:a (:b nil nil) nil)) false))
      (is (= (f '(:a (:b nil nil) (:c nil nil))) false))
      (is (= (f [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
        true))
      (is (= (f [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
        false))
      (is (= (f [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                [2 [3 nil [4 [6 nil nil] nil]] nil]])
        false))
    )
  )
  (testing "97, Pascal's Triangle"
    (let [f (fn [row]
      (nth
        (iterate #(concat '(1) (map + % (rest %)) '(1)) [1])
        ;; on initial [1], (map + [1] []) = ()
        (dec row)
      ))]
      (is (= (f 1) [1]))
      (is (= (map f (range 1 6))
        [   [1]
           [1 1]
          [1 2 1]
         [1 3 3 1]
        [1 4 6 4 1]]))
      (is (= (f 11)
        [1 10 45 120 210 252 210 120 45 10 1]))
    )
  )
  (testing "98, Equivalent Classes"
    (let [f (fn [f xs]
      (set (map #(-> % second set) (group-by f xs))))
      ;; #(into #{} (map (comp set second) (group-by % %2))) ; works too
    ]
      (is (= (f #(* % %) #{-2 -1 0 1 2})
        #{#{0} #{1 -1} #{2 -2}}))
      (is (= (f #(rem % 3) #{0 1 2 3 4 5 })
        #{#{0 3} #{1 4} #{2 5}}))
      (is (= (f identity #{0 1 2 3 4})
        #{#{0} #{1} #{2} #{3} #{4}}))
      (is (= (f (constantly true) #{0 1 2 3 4})
        #{#{0 1 2 3 4}})) 
    )
  )
  (testing "99, Result as a sequence of digits of the product"
    (let [f (fn [a b]
      (map #(- (int %) (int \0)) (str (* a b)))
    )]
      (is (= (f 1 1) [1]))
      (is (= (f 99 9) [8 9 1]))
      (is (= (f 999 99) [9 8 9 0 1])) 
    )
  )
)
(run-tests)
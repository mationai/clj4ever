(ns user
  (:require [clojure.test :refer [testing is run-tests deftest]]
            [clojure.string]))

(deftest problem-100s
  (testing "100, LCM"
    (let [f (fn [& xs]
      (letfn [
        (gcd [a b] (if (zero? b) a (recur b (mod a b))))
        (lcm [a b] (* (/ a (gcd a b)) b))]
      (reduce lcm xs)))]
      (is (== (f 2 3) 6))
      (is (== (f 3 12) 12))
      (is (== (f 12 3) 12))
      (is (== (f 5 7) 35))
      (is (== (f 7 5) 35))
      (is (== (f 5 3 7) 105))
      (is (== (f 1/3 2/5) 2))
      (is (== (f 3/4 1/6) 3/2))
      (is (== (f 7 5/7 2 3/5) 210))
    )
  )
  (testing "101, Hard - Levenshtein Distance"  
    ;; Given 2 sequences x y, calculate Levenshtein distance - the minimum #
    ;;  of edits needed to transform x into y. The allowed edits are:
    ;; - insert a single item 
    ;; - delete a single item 
    ;; - replace a single item with another item
    (let [f (fn [a b]
      ((fn lvn [a b i j]
        (if (or (zero? i) (zero? j))
          (max i j)
          (if (= (nth a (dec i)) (nth b (dec j)))
            (lvn a b (dec i) (dec j))
            (inc
              (min
              (lvn a b (dec i) j) ;; delete
              (lvn a b i (dec j)) ;; insert
              (lvn a b (dec i) (dec j)))) ;; replace
          )))
      a b (count a) (count b))
    )]
      (is (= (f "kitten" "sitting") 3))
      (is (= (f "closure" "clojure") (f "clojure" "closure") 1))
      (is (= (f "xyx" "xyyyx") 2))
      (is (= (f "" "123456") 6))
      (is (= (f "Clojure" "Clojure") (f "" "") (f [] []) 0))
      (is (= (f [1 2 3 4] [0 2 3 4 5]) 2))
      (is (= (f '(:a :b :c :d) '(:a :d)) 2))
      (is (= (f "ttttattttctg" "tcaaccctaccat") 10))
      (is (= (f "gaattctaatctc" "caaacaaaaaattt") 9))
    )
  )
  (testing "102, toCamelCase"  
    (let [f (fn [word]
      (let [words (clojure.string/split word #"-")]
        (str (first words)
          (->>
            (rest words)
            (map clojure.string/capitalize) clojure.string/join))
      ))]
      (is (= (f "hello-world") "helloWorld"))
      (is (= (f "something") "something"))
      (is (= (f "multi-word-key") "multiWordKey"))
      (is (= (f "leaveMeAlone") "leaveMeAlone"))
    )
  )
  (testing "103, Generate k-combinations of n"  
    ;; Given a sequence S consisting of n elements generate all k-combinations of S
    ;; i.e. generate all possible sets consisting of k distinct elements taken from S.
    ;; The number of k-combinations for a sequence is equal to the binomial coefficient.
    (let [f (fn rf [k sq]
      (cond
        (zero? k) #{#{}}
        (empty? sq) #{}
        :else (set (for [
          a sq
          b (rf (dec k) (disj sq a))]
                  (conj b a)))
      ))]
      (is (= (f 1 #{4 5 6}) #{#{4} #{5} #{6}}))
      (is (= (f 10 #{4 5 6}) #{}))
      (is (= (f 2 #{0 1 2}) #{#{0 1} #{0 2} #{1 2}}))
      (is (= (f 3 #{0 1 2 3 4})
              #{#{0 1 2} #{0 1 3} #{0 1 4} #{0 2 3} #{0 2 4}
                #{0 3 4} #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4}}))
      (is (= (f 4 #{[1 2 3] :a "abc" "efg"})
              #{#{[1 2 3] :a "abc" "efg"}}))
      (is (= (f 2 #{[1 2 3] :a "abc" "efg"})
              #{#{[1 2 3] :a} #{[1 2 3] "abc"} #{[1 2 3] "efg"}
                #{:a "abc"} #{:a "efg"} #{"abc" "efg"}}))
    )
  )
  (testing "104, Number to Roman Numerals"  
    (let [f (fn rn [n] 
      (let [[v s]
        (some
          #(cond (<= (first %) n) %) 
            [[3000 "MMM"] [2000 "MM"] [1000 "M"] [900 "CM"]
            [500 "D"] [400 "CD"] [300 "CCC"] [200 "CC"] [100 "C"]
            [90 "XC"] [50 "L"] [40 "XL"] [30 "XXX"] [20 "XX"] [10 "X"]
            [9 "IX"] [5 "V"] [4 "IV"] [3 "III"] [2 "II"] [1 "I"]]
        )]
        (if (nil? v) ""
            (str s (rn (- n v))))
      ))]
      (is (= "I" (f 1)))
      (is (= "XXX" (f 30)))
      (is (= "IV" (f 4)))
      (is (= "CXL" (f 140)))
      (is (= "DCCCXXVII" (f 827)))
      (is (= "MMMCMXCIX" (f 3999)))
      (is (= "XLVIII" (f 48)))
    )
  )
)

(run-tests)
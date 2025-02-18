(ns user
  (:require [clojure.test :refer [testing is run-tests deftest]]
            [clojure.string]))

(deftest sequences-6
  (testing "70, Sort words in a sentence, case insensitive"  
    (let [f #(sort-by (fn [v](.toLowerCase v))  (re-seq #"\w+" %))] 
      (is (= (f "Have a nice day.")
        ["a" "day" "Have" "nice"]))
      (is (= (f "Clojure is a fun language!")
        ["a" "Clojure" "fun" "is" "language"]))
      (is (= (f "Fools fall for foolish follies.")
        ["fall" "follies" "foolish" "Fools" "for"]))
    )
  )
  (testing "71, Rearranging Code using ->"  
    (let [f last] 
      (is (=
        (f (sort (rest (reverse [2 5 4 1 3 6]))))
        (-> [2 5 4 1 3 6] reverse rest sort f)
        5 )))
  )
  (testing "71, Rearranging Code using ->>"  
    ;; ->> for functions taking data as the last arg, like filter/map/reduce
    (let [f #(apply + %)] 
      (is (=
        (f (map inc (take 3 (drop 2 [2 5 4 1 3 6])))) ;; drop 2 drops 1st 2
        (->> [2 5 4 1 3 6] (drop 2) (take 3) (map inc) (f))
        11 )))
  )
  (testing "72, Analyze a TTT Board"  
    (let [f (fn [board]
      (first 
      (some #{[:o :o :o] [:x :x :x]}
        (concat
          board ;; rows
          (apply map list board) ;; cols (map list makes list of 1st,2nd,3rd items of each row)
          (map #(map (fn [f v] (f v)) board %) [[0 1 2] [2 1 0]]))))) ;; get diags
          ;; ^ other map works on each diag indices pattern
          ;; inner map on [0 1 2]:
          ;; map (fn [f v] (f v)) board [0 1 2]):
          ;;   (board[0] 0) - upper left 
          ;;   (board[1] 1) - middle 
          ;;   (board[2] 2) - lower right
    ] 
      (is (= nil (f [[:e :e :e]
                     [:e :e :e]
                     [:e :e :e]])))
      (is (= :x  (f [[:x :e :o]
                     [:x :e :e]
                     [:x :e :o]])))
      (is (= :o  (f [[:e :x :e]
                     [:o :o :o]
                     [:x :e :x]])))
      (is (= nil (f [[:x :e :o]
                     [:x :x :e]
                     [:o :x :o]])))
      (is (= :x  (f [[:x :e :e]
                     [:o :x :e]
                     [:o :e :x]])))
      (is (= :o  (f [[:x :e :o]
                     [:x :o :e]
                     [:o :e :x]])))
      (is (= nil (f [[:x :o :x]
                     [:x :o :x]
                     [:o :x :o]])))
    )
  )
  (testing "74, Filter Perfect Squares in a String"  
    (let [f (fn [s] (->> s 
      (re-seq #"\d+") 
      (map read-string) 
      (filter #(== (int (Math/sqrt %)) 
                  (Math/sqrt %))) 
      (clojure.string/join ",") ))] 
      (is (= (f "4,5,6,7,8,9") "4,9"))
      (is (= (f "15,16,25,36,37") "16,25,36"))
    )
  )
  (testing "75, Euler's Totient Function - coprimes"
    ;; 2 #s are coprime if their GCD = 1. Euler's totient function f(x) is
    ;;  defined as the # of positive integers less than x are coprime
    ;;  to x. The special case f(1) equals 1.
    ;; Coprimes doesn't have to be prime, see below
    (let [f (fn [x]
      (let [
        gcd (fn [a b] (if (zero? b) a (recur b (mod a b)))) ;; see #66
        q (fn [a] (= 1 (gcd x a)))]
      (count (filter q (range x)))))]
      (is (= (f 1) 1))
      (is (= (f 10) (count '(1 3 7 9)) 4))
      (is (= (f 40) 16)) ;; 1 3 7 9 11 13 17 19 21 23 27 29 31 33 37 39
      ;; note that 9 is not a prime, but is a coprime to 40
      (is (= (f 99) 60))
    )
  )
  (testing "76, Guess sequence generated by Trampolining 2 functions"  
    ;; Trampoline function takes a function f and a variable number of parameters.
    ;; Trampoline calls f with any parameters that were supplied.
    ;; If f returns a function, trampoline calls that function with no arguments.
    ;; This is repeated until the return value is not a function, and trampoline
    ;;  returns that non-function value. This is useful for implementing mutually
    ;;  recursive algorithms in a way that won't consume the stack.
    (is (= 
      [1 3 5 7 9 11] 
      (letfn [
        (foo [x y] #(bar (conj x y) y))
        (bar [x y] (if (> (last x) 10)
                    x
                    #(foo x (+ 2 y))))]
        (trampoline foo [] 1))))
    ;; 1. conj 1 to [] returns [1]
    ;; 2. bar [1] 1 returns a function
    ;; 3. trampoline calls that function with no args, which returns [1 3] ...
  )
  (testing "77, Anagram Finder"  
    ;; A word x is an anagram of word y if all the letters in x can be rearranged
    ;;  in a different order to form y.
    (let [f (fn [s]
      (set (map set ;; required in problem description 
        (filter #(> (count %) 1)
          (vals (group-by frequencies s))))))] 
      ;; Eg. s = ["meat" "mat" "team" "mate" "eat"]:
      ;; (frequencies s) -> {"meat" 1 "mat" 1 "team" 1 "mate" 1 "eat" 1}
      ;; but (group-by frequencies s) -> {
      ;;  {\m 1, \e 1, \a 1, \t 1} ["meat" "team" "mate"],
      ;;  {\m 1, \a 1, \t 1} ["mat"],
      ;;  {\e 1, \a 1, \t 1} ["eat"]}
      ;; (vals ...) -> (["meat" "team" "mate"] ["mat"] ["eat"])
      (is (= (f ["meat" "mat" "team" "mate" "eat"])
        #{#{"meat" "team" "mate"}}))
      (is (= (f ["veer" "lake" "item" "kale" "mite" "ever"])
        #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}}))
    )
  )
  (testing "78, Trampoline, w/o trampoline"  
    (let [trampoline (fn tr [f & args]
      (if (fn? f)
          (recur (apply f args) [])
          f ) )] 
      (is (=
        (letfn [
          (triple [x] #(sub-two (* 3 x)))
          (sub-two [x] #(stop?(- x 2)))
          (stop? [x] (if (> x 50) x #(triple x)))]
          (trampoline triple 2))
        82))
      (is (=
        (letfn [
          (my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
          (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
          (map (partial trampoline my-even?) (range 6)))
        [true false true false true false]))
    )
  )
  (testing "79, Hard - Triangle Minimal Path"
    ;; Function that calculates sum of minimal path through a triangle.
    ;; The triangle is represented as a vector of vectors. The path should
    ;;  start at the top of the triangle and move to an adjacent number on
    ;;  the next row until the bottom of the triangle is reached.

    (let [f (fn r [t]
      (if (first t)
        (+ (ffirst t) ;; first of first of [[1]...] is 1
           (min (r (map rest (rest t))) ;; R path, drop 1st elem of each row
                (r (map butlast (rest t))))) ;; L path, drop last elem of each row
          0 ))]
      (is (=
        (f [[1]
           [2 4]
          [5 1 4]
         [2 3 4 5]])
        (+ 1 2 1 3)
        7))
      (is (=
        (f   [[3]
             [2 4]
            [1 9 3]
           [9 9 2 4]
          [4 6 6 7 8]
         [5 7 3 5 1 4]])
        (+ 3 4 3 2 7 1)
        20))
    )
  )
)

(run-tests)
(ns user
  (:require [clojure.test :refer [testing is run-tests deftest]]))

(deftest problem-80s
  (testing "80, Perfect Numbers"
    ;; A number is "perfect" if the sum of its divisors equal the number itself.
    ;; 6 is a perfect number because 1+2+3=6.
    (let [f (fn [n]
      (= n (apply + (filter #(= 0 (mod n %)) (range 1 n)))))]
      (is (= (f 6) true))
      (is (= (f 7) false))
      (is (= (f 496) true))
      (is (= (f 500) false))
      (is (= (f 8128) true))
    )
  )
  (testing "81, Set Intersection, w/o intersection"
    (let [f (fn [x y]
        (set
          (filter #(contains? y %) x)))]
      (is (= (f #{0 1 2 3} #{2 3 4 5}) #{2 3}))
      (is (= (f #{0 1 2} #{3 4 5}) #{}))
      (is (= (f #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d}))
    )
  )
  (testing "82, Hard - If all words in input can form a Word Chain"
    ;; A word chain is a sequence of words where each word differs by one
    ;;  character from the previous word.
    (let [f (fn [words]
      (let [
        dist (fn dist [w u]
          (cond (empty? w) (count u)
                (empty? u) (count w)
                :else (min
                  (inc (dist (drop-last w) u))
                  (inc (dist w (drop-last u)))
                  (+ (dist (drop-last w) (drop-last u))
                    (if (= (last w) (last u)) 0 1)))
          ))
        adj (fn [w] ;filter ones in words that differ by one
          (->> words (filter #(= 1 (dist w %)))))
        lookup (->> words ;create lookup table
          (mapcat (juxt identity adj)) 
          (apply hash-map)) ;; for each word:
          ;; juxt... creates pairs of [word, adjacent-words]
          ;;  eg. (["spout" ["spot" "pout"]] ["do" ["dot"]] ...)
          ;; mapcat then concat flattens one level:
          ;;  eg. ("spout" ["spot" "pout"] "do" ["dot"])
          ;; apply hash-map  result:
          ;;  {"spout" ["spot" "pout"]
          ;;   "spot"  ["spout" "pot"]
          ;;   "pot"   ["spot" "pout" "dot"]
          ;;   "pout"  ["spout" "pot"]
          ;;   "dot"   ["pot" "do"]
          ;;   "do"    ["dot"]}
        path? (fn path? [seen w]
          (cond
            (= (count (conj seen w)) (count words)) true
            (seen w) false
            :else (some true? (map #(path? (conj seen w) %) (lookup w)))
          ))] ;; true for "pout" in 2nd true eg.
      (not-every? nil? (map #(path? #{} %) words))))]

      (is (= true  (f #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"})))
      ;; cat -> cot -> coat -> oat -> hat -> hot -> hog -> dog
      (is (= false (f #{"cot" "hot" "bat" "fat"})))
      (is (= false (f #{"to" "top" "stop" "tops" "toss"})))
      (is (= true  (f #{"spout" "do" "pot" "pout" "spot" "dot"})))
      ;; pout -> spout -> spot -> pot -> pout -> dot -> do
      (is (= true  (f #{"share" "hares" "shares" "hare" "are"})))
      (is (= false (f #{"share" "hares" "hare" "are"})))
    )
  )
  (testing "83, True if some params are true, but not all"
    (let [f (fn [& xs]
      (and
        (not-every? false? xs)
        (not-every? true? xs))
    )]
      (is (= false (f false false)))
      (is (= true  (f true false)))
      (is (= false (f true)))
      (is (= true  (f false true false)))
      (is (= false (f true true true)))
      (is (= true  (f true true true false)))
    )
  )
  (testing "84, Transitive Closure"
    ;; Return the transitive closure of a binary relation. The relation will be
    ;;  represented as a set of 2 item vectors.
    (let [f (fn [pairs-input]
      (let [add-transitive (fn [pairs]
        (let [new-pair
          (for [[a b] pairs [c d] pairs :when (= b c)]
            [a d])] ;; add new pair of [a d] if b = c, [] if not
          (into pairs new-pair)
        ))]
        (loop [pairs pairs-input prev-pairs #{}]
          (if (= pairs prev-pairs) ;; eq if no change
            pairs
            (recur (add-transitive pairs) pairs)))))]

      (let [
        divides #{[8 4] [9 3] [4 2] [27 9]}]
        (is (= (f divides)
          #{[4 2] [8 4] [8 2] [9 3] [27 9] [27 3]})))
      (let [
        more-legs #{["cat" "man"] ["man" "snake"] ["spider" "cat"]}]
        (is (= (f more-legs)
          #{["cat" "man"] ["cat" "snake"] ["man" "snake"]
            ["spider" "cat"] ["spider" "man"] ["spider" "snake"]})))
      (let [
        progeny #{["father" "son"] ["uncle" "cousin"] ["son" "grandson"]}]
        (is (= (f progeny)
          #{["father" "son"] ["father" "grandson"]
            ["uncle" "cousin"] ["son" "grandson"]})))
    )
  )
  (testing "85, Power Set - all subsets, including empty set and x itself"
    (let [f (fn [in-set]
      (let [
        res (for [a in-set b (disj in-set a)]
          #{a b #{a b}})]
        #{(into res (apply concat res))}))]
        ;; (into res #{} (apply concat res))))]
    ;;   (is (= (f #{1 :a}) #{#{1 :a} #{:a} #{} #{1}}))
      (println (f #{}))
    ;;   (is (= (f #{}) #{#{}}))
    ;;   (is (= (f #{1 2 3})
    ;;      #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}}))
    ;;   (is (= (count (f (into #{} (range 10)))) 1024))
    )
  )
;;   (testing "80, "
;;     (let [f ]
;;       (is )
;;     )
;;   )
;;   (testing "80, "
;;     (let [f ]
;;       (is )
;;     )
;;   )
;;   (testing "80, "
;;     (let [f ]
;;       (is )
;;     )
;;   )
;;   (testing "80, "
;;     (let [f ]
;;       (is )
;;     )
;;   )
)

(run-tests)
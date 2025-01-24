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
        (let [
          new-pair (for [[a b] pairs [c d] pairs :when (= b c)]
            [a d] )] ;; all missing transitive pairs will be added, [] if none
          (into pairs new-pair)
        ))]
        (loop [pairs pairs-input prev-pairs #{}]
          (if (= pairs prev-pairs) ;; is = if no change (last iteration)
            pairs
            (recur (add-transitive pairs) pairs))
        )))]

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
      (if (empty? in-set) #{#{}}
      (let [sorted (vec (sort-by hash in-set))] 
        (reduce
          (fn [acc i] 
            (into acc (for [subset acc]
                (conj subset (nth sorted i)))))
          #{#{} in-set} ; start with empty and full set
          (range (count sorted)))
      )))]

      (is (= (f #{1 :a}) #{#{1 :a} #{:a} #{} #{1}}))
      (is (= (f #{}) #{#{}}))
      (is (= (f #{1 2 3})
        #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}}))
      (is (= (count (f (into #{} (range 10)))) 1024))
    )
  )
  (testing "86, Happy Numbers"
    ;; Take each digit of a #, square it, and sum the squares to get a new #.
    ;; Repeat with the new # and eventually, you might get to a # whose
    ;;  squared sum is 1. The input and all intermediate #s are happy numbers.
    ;; An unhappy # will loop forever.
    (let [f (fn [n]
      (loop [n n, mem #{}]
        (let [
          digits (map (comp read-string str) (str n))
          sum (apply + (map #(* % %) digits))]
          (or (= sum 1)
              (if (mem sum) false (recur sum (conj mem sum)))))
      ))]
      (is (= (f 7) true))
      (is (= (f 986543210) true))
      (is (= (f 2) false))
      (is (= (f 3) false))
    )
  )
  (testing "87, Create a function that generate code representing an equation"
    ;; The following rules for the equation must be satisfied:
    ;; 0. Takes 3+ integers
    ;; 1. All integers must be used once and only once.
    ;; 2. The order of the integers must be maintained when reading the equation
    ;; 3. The only functions you may use are +, *, or =. 
    ;; 4. The equation must use the minimum number of parentheses. 
    ;; 5. If no satisfying equation exists, return nil.
    (is true)
  )
  (testing "88, Symmetric Difference of 2 sets"
    (let [f (fn [x y]
      (let [
        diff (fn [x y]
          (set (filter #(not (contains? y %)) x)))]
        (if (= x y) #{} (into (diff x y) (diff y x)))
      )
    )]
      (is (= (f #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7}))
      (is (= (f #{:a :b :c} #{}) #{:a :b :c}))
      (is (= (f #{} #{4 5 6}) #{4 5 6}))
      (is (= (f #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]}))
    )
  )
  (testing "89, Graph"
    ;; Returns true if tour is possible to visit every edge exactly once.
    ;; The graph is represented by a vector of tuples, where each tuple
    ;;  represents a single edge. The rules are:
    ;; - You can start at any node.
    ;; - You must visit each edge exactly once.
    ;; - All edges are undirected.
    (let [f (fn [edges] ; eg 1.[[a b] [b c]] [c a]]    eg 2. [[a b] [a c]]
      (let [
        nodes-via-edges (flatten edges) ;(a b b c c a)       (a b a c)
        nodes (set nodes-via-edges)     ;#{a b c}            #{a c}
        edges-set (set (map set edges)) ;#{{a b} {b c} {c a} #{{a b} {a c}}
        node-counts (->> (group-by identity nodes-via-edges)  ; get node sink counts
        ;; 1. {a: [a a]    2. {a: [a a]
        ;;     b: [b b]        b: [b]
        ;;     c: [c c]}       c: [c]
          (map (fn [[_ v]] (count v)))) ;val counts (2 2 2)  (2 1 1)
      ] 
      (if (= 2 (count nodes))     ; special case: only 2 nodes total
        (= edges-set (conj edges-set nodes))  ; check if 1 edge connects them
        ;; could be 2 discounntected circular edges to each node 
        ;; circular edges-set eg: {a a} {b b}, conj nodes of {a b} won't be eq. 
        ;; but 2 nodes of {a b} will have edges-set of {a b} or {b a} so conj will eq.
        (every? even? node-counts))))  ; else, each node must have even degree
      ]
      (is (= true (f [[:a :b]])))
      (is (= false (f [[:a :a] [:b :b]])))
      (is (= false (f [[:a :b] [:a :b] [:a :c] [:c :a]
                   [:a :d] [:b :d] [:c :d]])))
      (is (= true (f [[1 2] [2 3] [3 4] [4 1]])))
      (is (= true (f [[:a :b] [:a :c] [:c :b] [:a :e]
                    [:b :e] [:a :d] [:b :d] [:c :e]
                    [:d :e] [:c :f] [:d :f]])))
      (is (= false (f [[1 2] [2 3] [2 4] [2 5]])))
    )
  )
)

(run-tests)
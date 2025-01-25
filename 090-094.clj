(ns user
  (:require [clojure.test :refer [testing is run-tests deftest]]
            [clojure.set :as cljset]))

(deftest problem-90s
  (testing "90, Cartesian Product of 2 sets"
    (let [f (fn [a b]
      (set
        (for [i a j b] [i j])))]
      (is (= (f #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
        #{["ace"   "♠"] ["ace"   "♥"] ["ace"   "♦"] ["ace"   "♣"]
          ["king"  "♠"] ["king"  "♥"] ["king"  "♦"] ["king"  "♣"]
          ["queen" "♠"] ["queen" "♥"] ["queen" "♦"] ["queen" "♣"]}))
      (is (= (f #{1 2 3} #{4 5})
        #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]}))
      (is (= 300 (count (f (into #{} (range 10))
                        (into #{} (range 30))))))
    )
  )
  (testing "91, Graph Connectivity"
    (let [f (fn [edges]
      (let [
        connect (fn [acc-nodes edge] 
          (let [
            pair (set edge), 
            conn-to-edge  (filter #(not-empty (cljset/intersection % pair)) acc-nodes), 
            uncon-to-edge (filter #(empty?    (cljset/intersection % pair)) acc-nodes)
            new-acc (conj uncon-to-edge
                  (apply cljset/union (conj conn-to-edge pair)))]
            ;; (println "\nEdge:" edge)
            ;; (println "acc Nodes:" acc-nodes)
            ;; (println "Connected to edge:" conn-to-edge)
            ;; (println "Unconnected to edge:" uncon-to-edge)
            ;; (println "New acc nodes:" new-acc)
            new-acc
          ))]
        (= 1 (count (reduce connect [] edges))) ;; all connected if islands = 1
      ))]
      ;; Eg. [[1 2] [3 4][4 5]] - 2 separate islands
      ;; Edge: [1 2]
      ;; acc-nodes: []
      ;; connected to edge: []
      ;; unconnected to edge: []
      ;; new acc nodes: [#{1 2}]

      ;; Edge: [3 4]
      ;; acc-nodes: [#{1 2}]
      ;; connected to edge: []
      ;; unconnected to edge: [#{1 2}]
      ;; new acc nodes: [#{1 2} #{3 4}]

      ;; Edge: [4 5]
      ;; acc-nodes: [#{1 2} #{3 4}]
      ;; connected to edge: [#{3 4}]
      ;; unconnected to edge: [#{1 2}]
      ;; new acc nodes: [#{1 2} #{3 4 5}]
      (is (= true (f #{[:a :a]})))
      (is (= true (f #{[:a :b]})))
      (is (= false (f #{[1 2] [2 3] [3 1]
                    [4 5] [5 6] [6 4]})))
      (is (= true (f #{[1 2] [2 3] [3 1]
                    [4 5] [5 6] [6 4] [3 4]})))
      (is (= false (f #{[:a :b] [:b :c] [:c :d]
                    [:x :y] [:d :a] [:b :e]})))
      (is (= true (f #{[:a :b] [:b :c] [:c :d]
                    [:x :y] [:d :a] [:b :e] [:x :a]}))) 
    )
  )
  (testing "92, Roman Numerals"
    ;; No need to handle #s > MMMCMXCIX (3999), the largest representable #
    (let [f (fn [s]
      (let [r->i {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1}]
        (+ (apply +
             (map (fn [[fst snd]]
               (if (< (r->i fst) (r->i snd)) ;; if val of fst < snd
                 (* -1 (r->i fst)) ; negate 1st, eg. I V = -1 + 5
                 (r->i fst))
               ) (partition 2 1 s)) ;; overlap sliding window,
               ;; eg. "XIV"->([\X \I] [\I \V])
           ) (r->i (last s))) ;; last val not added yet, add it, always positive
      ))]
      (is (= 14 (f "XIV")))
      (is (= 827 (f "DCCCXXVII")))
      (is (= 3999 (f "MMMCMXCIX")))
      (is (= 48 (f "XLVIII"))) )
  )
  (testing "93, Flatten a Sequence til only 1 nested level"
    (let [f (fn rf [coll]
      (if (coll? (first coll)) ; is first element a collection?
        (mapcat rf coll)       ; if yes: recursively flatten each element
        [coll]))]              ; else wrap input in vector
      (is (= (f [["Do"] ["Nothing"]])
        [["Do"] ["Nothing"]]))
      (is (= (f [[[[:a :b]]] [[:c :d]] [:e :f]])
        [[:a :b] [:c :d] [:e :f]]))
      (is (= (f '((1 2)((3 4)((((5 6)))))))
        '((1 2)(3 4)(5 6))))
      ;; ((1 2)) -> else path -> ['(1 2)]
      ;; ((3 4)((((5 6)))) -> recur both -> ['(3 4)] and ['(5 6)] 
      ;; mapcat calls map on rf, then concats them, which removes the wrapping vector
    ) 
  )
  (testing "94, Game of Life - return next generation where"
    ;; 1) live cell with < 2 live neighbours dies.
    ;; 2) live cell with 2 or 3 live neighbours lives.
    ;; 3) live cell with > 3 live neighbours dies.
    ;; 4) dead cell with 3 live neighbours becomes a live cell
    (let [f (fn [board]
    (map-indexed
      (fn [i row-str]
        (apply str (map-indexed
          (fn [j s]
            (->>
              (for [x [-1 0 1] y [-1 0 1] :when (not= [x y] [0 0])] [x y])
              ;; gen all 8 neighbors, map + to each neighbor
              (map (partial map + [i j])) ;; map coord to each neighbor for each cell
              (filter (fn [[k-row l-col]] ;; filter out out-of-bounds neighbors
                (and (< -1 k-row (count board)) ;; k > -1 && k < num rows
                    (< -1 l-col (count (first board)))))) ;; check num cols
              (map (fn [[k-row l-col]]    ;; map neighbors to board vals
                ((vec (board k-row)) l-col))) ;; get l-col val from vector of k-row chars
              (filter (partial = \#))
              (count) ;; count live neighbors
              (#(if (= s \#)
                  (cond
                    (or (> % 3) (< % 2)) \space ;; dies
                    :else    \#) ;; lives
                  (if (= % 3) \# \space))) ;; dead -> live or stays dead
              )
          ) row-str ))
      )
      board ))]
      (is (= (f [
          "      "
          " ##   "
          " ##   "
          "   ## "
          "   ## "
          "      "])
         ["      "
          " ##   "
          " #    "
          "    # "
          "   ## "
          "      "]))
      (is (= (f [
          "     "
          "     "
          " ### "
          "     "
          "     "])
         ["     "
          "  #  "
          "  #  "
          "  #  "
          "     "]))
      (is (= (f [
          "      "
          "      "
          "  ### "
          " ###  "
          "      "
          "      "])
         ["      "
          "   #  "
          " #  # "
          " #  # "
          "  #   "
          "      "]))
    )
  )
)
(run-tests)
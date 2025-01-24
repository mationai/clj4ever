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
        (= 1 (count
          (reduce connect [] edges)))
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
  ;; (testing "90, "
  ;;   (let [f ]
  ;;     (is ) 
  ;;   )
  ;; )
  ;; (testing "90, "
  ;;   (let [f ]
  ;;     (is ) 
  ;;   )
  ;; )
  ;; (testing "90, "
  ;;   (let [f ]
  ;;     (is ) 
  ;;   )
  ;; )
  ;; (testing "90, "
  ;;   (let [f ]
  ;;     (is ) 
  ;;   )
  ;; )
  ;; (testing "90, "
  ;;   (let [f ]
  ;;     (is ) 
  ;;   )
  ;; )
  ;; (testing "90, "
  ;;   (let [f ]
  ;;     (is ) 
  ;;   )
  ;; )
  ;; (testing "90, "
  ;;   (let [f ]
  ;;     (is ) 
  ;;   )
  ;; )
  ;; (testing "90, "
  ;;   (let [f ]
  ;;     (is ) 
  ;;   )
  ;; )
)
(run-tests)
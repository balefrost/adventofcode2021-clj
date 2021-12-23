(ns adventofcode2021.day23
  (:use adventofcode2021.advent-util)
  (:use clojure.pprint)
  (:use [clojure.data.priority-map :only [priority-map]])
  (:require [clojure.string :as str]))

(def valid-intermediates
  #{[1 1]
    [1 2]
    [1 4]
    [1 6]
    [1 8]
    [1 10]
    [1 11]})

(def desired-chutes
  {\A 3
   \B 5
   \C 7
   \D 9})

(def step-costs
  {\A 1
   \B 10
   \C 100
   \D 1000})

(defn generate-path [from to]
  (let [[fromy fromx] from
        [toy tox] to
        _ (assert (not= fromx tox))
        xincr (sign (- tox fromx))
        rise (for [y (range (dec fromy) 0 -1)]
               [y fromx])
        run (for [x (range (+ fromx xincr) (+ tox xincr) xincr)]
              [1 x])
        fall (for [y (range 2 (inc toy))]
               [y tox])]
    (concat rise run fall)))

(defn has-open-path [state path]
  (not-any? #(contains? state %) path))

(defn compute-cost [path occupant]
  (* (count path) (get step-costs occupant)))

(defn chute-contents [state chute]
  (for [[[y x] occupant] state
        :when (> y 1)
        :when (= x chute)]
    occupant))

(defn available-launches [chute-height state]
  (for [x [3 5 7 9]
        :let [top-of-chute (first (for [y (range 2 (+ 2 chute-height))
                                        :let [pos [y x]]
                                        :when (contains? state pos)]
                                    pos))]
        :let [[y _] top-of-chute]
        :when top-of-chute
        :let [occupant (get state top-of-chute)]
        :let [desired-chute (get desired-chutes occupant)]
        :when (or
                (not= x desired-chute)
                (some
                  (fn [y]
                    (not= occupant (get state [y x])))
                  (range y (+ 2 chute-height))))]
    top-of-chute))

(defn available-landing [chute-height state symbol]
  (first
    (for [x [(get desired-chutes symbol)]
          :let [landing-pos (first (for [y (range (inc chute-height) 1 -1)
                                         :let [pos [y x]]
                                         :when (not (contains? state pos))]
                                     pos))]
          :when landing-pos
          :let [[y _] landing-pos]
          :when (every?
                  (fn [y]
                    (= symbol (get state [y x])))
                  (range (inc y) (+ 2 chute-height)))]
      landing-pos)))

(defn can-land-in-chute [state chute symbol]
  (and
    (= (get desired-chutes symbol) chute)
    (every? #(= % symbol) (chute-contents state chute))))

(defn can-launch-from-chute [state chute symbol]
  (or
    (not= (get desired-chutes symbol) chute)
    (some #(= % symbol) (chute-contents state chute))))

(defn chute-landing-pos [chute-height state chute]
  (first
    (for [y (range (inc chute-height) 1 -1)
          :let [pos [y chute]]
          :when (not (contains? state pos))]
      pos)))

(defn possible-moves [chute-height state]
  (let [possible-flights (for [from (available-launches chute-height state)
                               :let [occupant (get state from)]
                               :let [to (available-landing chute-height state occupant)]
                               :when to

                               :let [path (generate-path from to)]
                               :when (has-open-path state path)
                               :let [move-cost (compute-cost path occupant)]]
                           [(-> state
                                (dissoc from)
                                (assoc to occupant))
                            move-cost])
        possible-launches (for [from (available-launches chute-height state)
                                :let [occupant (get state from)]
                                to valid-intermediates

                                :let [path (generate-path from to)]
                                :when (has-open-path state path)
                                :let [move-cost (compute-cost path occupant)]]
                            [(-> state
                                 (dissoc from)
                                 (assoc to occupant))
                             move-cost])
        possible-landings (for [[[y _ :as from] occupant] state
                                :when (= y 1)

                                :let [to (available-landing chute-height state occupant)]
                                :when to

                                :let [path (generate-path from to)]
                                :when (has-open-path state path)
                                :let [move-cost (compute-cost path occupant)]]
                            [(-> state
                                 (dissoc from)
                                 (assoc to occupant))
                             move-cost])]
    (if (not (empty? possible-flights))
      possible-flights
      (concat possible-launches possible-landings))))

(defn dijkstra-seq [get-neighbors initial-state]
  (letfn [(helper [queue total-cost prev]
            (lazy-seq
              (cons
                {:queue      queue
                 :total-cost total-cost
                 :prev       prev}
                (if-let [[[state best-cost-for-state]] (seq queue)]
                  (let [queue (dissoc queue state)
                        neighbors-with-costs (get-neighbors state)
                        nonfinal-neighbors-with-costs (for [nwc neighbors-with-costs
                                                            :let [[neighbor _] nwc]
                                                            :when (not (contains? total-cost neighbor))]
                                                        nwc)
                        [queue prev] (loop [queue queue
                                            prev prev
                                            neighbors nonfinal-neighbors-with-costs]
                                       (if-let [[[n incr-cost] & neighbors] (seq neighbors)]
                                         (let [alt-total-cost (+ best-cost-for-state incr-cost)]
                                           (if (or (not (contains? queue n)) (< alt-total-cost (get queue n)))
                                             (recur (assoc queue n alt-total-cost) (assoc prev n state) neighbors)
                                             (recur queue prev neighbors)))
                                         [queue prev]))
                        total-cost (assoc total-cost state best-cost-for-state)]
                    (helper queue total-cost prev))))))]

    (helper
      (-> (priority-map)
          (assoc initial-state 0))
      {}
      {})))

(defn astar [get-neighbors initial-state target-state heuristic]
  (loop [queue (priority-map initial-state 0)
         total-cost {initial-state 0}
         prev {}]
    (if-let [[[state _]] (seq queue)]
      (if (= state target-state)
        {:queue      queue
         :total-cost total-cost
         :prev       prev}
        (let [queue (dissoc queue state)
              neighbors-with-costs (get-neighbors state)
              [queue total-cost prev] (loop [neighbors neighbors-with-costs
                                             queue queue
                                             total-cost total-cost
                                             prev prev]
                                        (if-let [[[n incr-cost] & neighbors] (seq neighbors)]
                                          (let [alt-total-cost (+ (get total-cost state) incr-cost)]
                                            (if (or (not (contains? total-cost n))
                                                    (< alt-total-cost (get total-cost n)))
                                              (recur
                                                neighbors
                                                (assoc queue n (+ alt-total-cost (heuristic n)))
                                                (assoc total-cost n alt-total-cost)
                                                (assoc prev n state))
                                              (recur
                                                neighbors
                                                queue
                                                total-cost
                                                prev)))
                                          [queue total-cost prev]))]
          (recur queue total-cost prev)))
      {:queue      queue
       :total-cost total-cost
       :prev       prev})))

(defn take-until [pred coll]
  (lazy-seq
    (if-let [[hd & coll] coll]
      (if (pred hd)
        [hd]
        (cons hd (take-until pred coll))))))

(def sample-initial-state {[2 3] \B
                           [3 3] \A

                           [2 5] \C
                           [3 5] \D

                           [2 7] \B
                           [3 7] \C

                           [2 9] \D
                           [3 9] \A})

(def sample-initial-state-2 {[2 3] \B
                             [3 3] \D
                             [4 3] \D
                             [5 3] \A

                             [2 5] \C
                             [3 5] \C
                             [4 5] \B
                             [5 5] \D

                             [2 7] \B
                             [3 7] \B
                             [4 7] \A
                             [5 7] \C

                             [2 9] \D
                             [3 9] \A
                             [4 9] \C
                             [5 9] \A})

(def my-initial-state {[2 3] \D
                       [3 3] \C

                       [2 5] \B
                       [3 5] \A

                       [2 7] \A
                       [3 7] \D

                       [2 9] \C
                       [3 9] \B})

(def my-initial-state-2 {[2 3] \D
                         [3 3] \D
                         [4 3] \D
                         [5 3] \C

                         [2 5] \B
                         [3 5] \C
                         [4 5] \B
                         [5 5] \A

                         [2 7] \A
                         [3 7] \B
                         [4 7] \A
                         [5 7] \D

                         [2 9] \C
                         [3 9] \A
                         [4 9] \C
                         [5 9] \B})

(defn state-distance-from-goal [state]
  (reduce +
          (for [[[_ x] occupant] state]
            (let [dist (abs-value (- x (get desired-chutes occupant)))
                  cost (* dist (get step-costs occupant))]
              cost))))

(defn part1 []
  (let [initial-state my-initial-state
        final-state {[2 3] \A
                     [3 3] \A

                     [2 5] \B
                     [3 5] \B

                     [2 7] \C
                     [3 7] \C

                     [2 9] \D
                     [3 9] \D}]
    (let [winning-state (astar
                          #(possible-moves 2 %)
                          initial-state
                          final-state
                          state-distance-from-goal)
          cost (get-in winning-state [:total-cost final-state])]
      cost)))

(defn part2 []
  (let [initial-state my-initial-state-2
        final-state {[2 3] \A
                     [3 3] \A
                     [4 3] \A
                     [5 3] \A

                     [2 5] \B
                     [3 5] \B
                     [4 5] \B
                     [5 5] \B

                     [2 7] \C
                     [3 7] \C
                     [4 7] \C
                     [5 7] \C

                     [2 9] \D
                     [3 9] \D
                     [4 9] \D
                     [5 9] \D}]
    (let [winning-state (astar
                          #(possible-moves 4 %)
                          initial-state
                          final-state
                          state-distance-from-goal)
          cost (get-in winning-state [:total-cost final-state])]
      cost)))

(defn -main []
  (try
    (do
      (println (part1))
      (println (part2)))
    (catch Exception e (.printStackTrace e))))

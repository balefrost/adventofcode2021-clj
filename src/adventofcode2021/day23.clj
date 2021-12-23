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
        _ (assert (not= fromy toy))
        xincr (sign (- tox fromx))]
    (cond
      (= fromy 1)
      (concat
        (for [x (range (+ fromx xincr) tox xincr)]
          [fromy x])
        (for [y (range fromy toy)]
          [y tox])
        [[toy tox]])

      (> fromy 1)
      (concat
        (for [y (range (dec fromy) toy -1)]
          [y fromx])
        (for [x (range fromx tox xincr)]
          [toy x])
        [[toy tox]]))))

(defn has-open-path [state path]
  (not-any? #(contains? state %) path))

(defn compute-cost [path occupant]
  (* (count path) (get step-costs occupant)))

(defn possible-moves [state]
  (let [possible-launches (for [[[y x :as from] occupant] state
                                :when (> y 1)
                                :when (or
                                        (not= x (get desired-chutes occupant))
                                        (or (and (= y 2)
                                                 (not= occupant (get state [3 x])))))

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
                                :let [tox (get desired-chutes occupant)]
                                toy [2 3]
                                :let [to [toy tox]
                                      path (generate-path from to)]
                                :when (has-open-path state path)
                                :let [move-cost (compute-cost path occupant)]]
                            [(-> state
                                 (dissoc from)
                                 (assoc to occupant))
                             move-cost])]
    (concat possible-launches possible-landings)))

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
                        [dist prev] (loop [queue queue
                                           prev prev
                                           neighbors nonfinal-neighbors-with-costs]
                                      (if-let [[[n incr-cost] & neighbors] (seq neighbors)]
                                        (let [alt-total-cost (+ best-cost-for-state incr-cost)]
                                          (if (< alt-total-cost (get queue n Float/POSITIVE_INFINITY))
                                            (recur (assoc queue n alt-total-cost) (assoc prev n state) neighbors)
                                            (recur queue prev neighbors)))
                                        [queue prev]))
                        total-cost (assoc total-cost state best-cost-for-state)]
                    (helper dist total-cost prev))))))]

    (helper
      (-> (priority-map)
          (assoc initial-state 0))
      {}
      {})))

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

(def my-initial-state {[2 3] \D
                       [3 3] \C

                       [2 5] \B
                       [3 5] \A

                       [2 7] \A
                       [3 7] \D

                       [2 9] \C
                       [3 9] \B})

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
    (let [winning-state (last (take-until #(contains? (:total-cost %) final-state) (dijkstra-seq possible-moves initial-state)))
          cost (get-in winning-state [:total-cost final-state])]
      cost)))

(defn part2 [])

(defn -main []
  (try
    (do
      (println (part1))
      (println (part2)))
    (catch Exception e (.printStackTrace e))))

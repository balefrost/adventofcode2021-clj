(ns adventofcode2021.day15
  (:use adventofcode2021.advent-util)
  (:use clojure.pprint)
  (:use [clojure.data.priority-map :only [priority-map]])
  (:require [clojure.string :as str]))

(defn process-input [input]
  (mapv
    (fn [line] (mapv (fn [c] (- (int c) (int \0))) line))
    (str/split-lines input)))

(def input (process-input (read-input)))
;(def input (process-input "1163751742\n1381373672\n2136511328\n3694931569\n7463417111\n1319128137\n1359912421\n3125421639\n1293138521\n2311944581"))

(defn dijkstra [all-vertices get-neighbors get-cost]
  (loop [dist (-> (priority-map)
                  (into (for [coord all-vertices]
                          [coord Integer/MAX_VALUE]))
                  (assoc [0 0] 0))
         prev {}]
    (if-let [[[coord v]] (seq dist)]
      (let [dist (dissoc dist coord)
            neighbors (filter #(contains? dist %) (get-neighbors coord))
            [dist prev] (loop [dist dist
                               prev prev
                               neighbors neighbors]
                          (if-let [[n & neighbors] (seq neighbors)]
                            (let [cost-to-enter (get-cost coord n)
                                  alt (+ v cost-to-enter)]
                              (if (< alt (get dist n))
                                (recur (assoc dist n alt) (assoc prev n coord) neighbors)
                                (recur dist prev neighbors)))
                            [dist prev]))]
        (recur dist prev))
      prev)))

(defn part1 []
  (let [dims (grid-dims input)
        prev (dijkstra
               (grid-coords input)
               (fn [coord] (grid-adjacent4 dims coord))
               (fn [_ to] (get-in input to)))
        lower-right (mapv dec dims)
        path (butlast (iterate-until-nil prev lower-right))]
    (reduce + (map #(get-in input %) path))))


(defn part2 []
  (let [[oh ow :as odims] (grid-dims input)
        dims (mapv (partial * 5) odims)
        get-value (fn [to]
                    (let [[toy tox] to
                          valoffset (+ (quot toy oh) (quot tox ow))
                          ocoord [(rem toy oh) (rem tox ow)]
                          ovalue (get-in input ocoord)]
                      (inc (rem (+ (dec ovalue) valoffset) 9))))
        all-coords (for [y (range (first dims))
                         x (range (second dims))]
                     [y x])
        prev (dijkstra
               all-coords
               (fn [coord] (grid-adjacent4 dims coord))
               (fn [_ to] (get-value to)))
        lower-right (mapv dec dims)
        path (butlast (iterate-until-nil prev lower-right))]
    (reduce + (map #(get-value %) path))))

(defn -main []
  (try
    (do
      (println (part1))
      (println (part2)))
    (catch Exception e (.printStackTrace e))))

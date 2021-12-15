(ns adventofcode2021.day09
  (:use adventofcode2021.advent-util)
  (:require [clojure.string :as str]))

(defn process-input [in]
  (mapv
    (fn [line]
      (mapv #(- (int %) (int \0)) line))
    (str/split-lines in)))

(def input (process-input (read-input)))
;(def input (process-input "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"))

(defn find-low-points [in]
  (let [dims (grid-dims in)]
    (for [pos (dims-iterate dims)
          :let [my-height (get-in in pos)
                adjacent-heights (map #(get-in in %) (grid-adjacent4 dims pos))
                bad-adjacent (filter #(<= % my-height) adjacent-heights)
                pos (if (zero? (count bad-adjacent))
                      pos)]
          :when pos]
      pos)))

(defn part1 []
  (reduce + 0
          (for [low-point (find-low-points input)
                :let [my-height (get-in input low-point)
                      my-risk (+ my-height 1)]]
            my-risk)))

(defn step-basins [basins]
  (let [dims (grid-dims input)]
    (for [basin basins
          :let [more-posns (sort (mapcat #(grid-adjacent4 dims %) basin))
                valid-posns (filter #(< (get-in input %) 9) more-posns)]]

      (into #{} (concat basin valid-posns)))))

(defn part2 []
  (let [final-config (last
                       (iterate-until-stable
                         step-basins
                         (map #(set [%]) (find-low-points input))))
        sorted (sort-by count final-config)
        best (take-last 3 sorted)]
    (reduce * (map count best))))


(defn -main []
  (println (part1))
  (println (part2)))

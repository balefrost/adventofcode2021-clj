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

(defn adjacent-locs [[y x] h w]
  (let [candidates [[y (dec x)]
                    [y (inc x)]
                    [(dec y) x]
                    [(inc y) x]]]
    (filter
      (fn [[y x]]
        (and
          (>= x 0)
          (< x w)
          (>= y 0)
          (< y h)))
      candidates)))

(defn part1 []
  (let [w (count (first input))
        h (count input)]
    (reduce + 0
            (for [x (range w)
                  y (range h)
                  :let [my-height (get-in input [y x])
                        adjacent-heights (map #(get-in input %) (adjacent-locs [ y x] h w))
                        bad-adjacent (filter #(<= % my-height) adjacent-heights)
                        my-risk (if (zero? (count bad-adjacent))
                                  (+ my-height 1))]
                  :when my-risk]
              my-risk))))

(defn find-low-points [in]
  (let [w (count (first in))
        h (count in)]
    (for [x (range w)
          y (range h)
          :let [my-height (get-in in [y x])
                adjacent-heights (map #(get-in in %) (adjacent-locs [ y x] h w))
                bad-adjacent (filter #(<= % my-height) adjacent-heights)
                pos (if (zero? (count bad-adjacent))
                      [y x])]
          :when pos]
      pos)))

(defn step-basins [basins]
  (let [w (count (first input))
        h (count input)]
    (for [basin basins
          :let [more-posns (sort (mapcat #(adjacent-locs % h w) basin))
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

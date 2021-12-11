(ns adventofcode2021.day11
  (:use adventofcode2021.advent-util)
  (:require [clojure.string :as str]))

(defn process-input [input]
  (mapv (fn [line] (mapv (fn [ch] (- (int ch) (int \0))) line)) (str/split-lines input)))

(def input (process-input (read-input)))
;(def input (process-input "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526"))

(defn grid-map [f grid]
  (mapv
    (fn [row]
      (mapv f row))
    grid))

(defn grid-coords [grid]
  (for [y (range (count grid))
        :let [row (nth grid y)]
        x (range (count row))]
    [y x]))

(defn grid-filter-coords [f grid]
  (filter
    (fn [coord]
      (f coord (get-in grid coord)))
    (grid-coords grid)))

(defn grid-all [pred grid]
  (every?
    (fn [coord]
      (pred coord (get-in grid coord)))
    (grid-coords grid)))

(defn grid-adjacent8 [coord]
  (let [[y x] coord]
    [[(dec y) (dec x)]
     [(dec y) x]
     [(dec y) (inc x)]
     [y (dec x)]
     [y (inc x)]
     [(inc y) (dec x)]
     [(inc y) x]
     [(inc y) (inc x)]]))

(defn grid-clip-coords [grid coords]
  (let [[h w] (grid-dims grid)]
    (filter
      (fn [[y x]]
        (and (<= 0 x (dec w))
             (<= 0 y (dec h))))
      coords)))

(defn flash-at [grid coord]
  (let [adj (grid-adjacent8 coord)
        valid (grid-clip-coords grid adj)]
    (reduce
      #(update-in %1 %2 inc)
      grid
      valid)))

(defn step [{grid :grid totalf :totalf}]
  (let [increased (grid-map inc grid)
        [grid-after-all-flashing flash-count] (loop [grid increased
                                                     flashed #{}]
                                                (let [max-energy (grid-filter-coords (fn [_ val] (> val 9)) grid)
                                                      new-flashed (remove flashed max-energy)]
                                                  (if (empty? new-flashed)
                                                    [grid (count flashed)]
                                                    (recur
                                                      (reduce
                                                        flash-at
                                                        grid
                                                        new-flashed)
                                                      (into flashed new-flashed)))))
        cooled (grid-map
                 (fn [v] (if (> v 9) 0 v))
                 grid-after-all-flashing)]
    {:grid   cooled
     :totalf (+ totalf flash-count)}))


(defn grid-print [grid]
  (dorun
    (for [row grid]
      (do
        (dorun
          (for [ch row]
            (print ch)))
        (println)))))

(defn part1 []
  (:totalf
    (nth (iterate step {:grid input :totalf 0}) 100)))

(defn part2 []
  (some
    identity
    (map
      (fn [{grid :grid} idx]
        (if (grid-all (fn [coord val] (= 0 val)) grid)
          idx))
      (iterate step {:grid input :totalf 0})
      (range))))


(defn -main []
  ;(grid-print (step (step (process-input "11111\n19991\n19191\n19991\n11111")))))
  (println (part1))
  (println (part2)))

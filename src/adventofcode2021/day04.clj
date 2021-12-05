(ns adventofcode2021.day04
  (:use adventofcode2021.advent-util)
  (:require [clojure.string :as str]))

(defn process-input [lines]
  (let [[numbers & lines] lines
        numbers (map parse-int (str/split numbers #","))
        boards (str/join " " lines)
        board-numbers (map parse-int (re-seq #"\d+" boards))
        boards (map vec (partition 25 board-numbers))]
    { :numbers numbers :boards boards}))

(def input (process-input (read-input-lines)))
;(def input (process-input (str/split-lines "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\n22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n 1 12 20 15 19\n\n 3 15  0  2 22\n 9 18 13 17  5\n19  8  7 25 23\n20 11 10 24  4\n14 21 16 12  6\n\n14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7")))

(defn rowcols [board]
  (concat
    (partition 5 board)
    (apply map vector (partition 5 board))))

(defn score-board [last-played board]
  (if (some #(every? nil? %) (rowcols board))
    (let [unmarked (filter (comp not nil?) board)]
      (* last-played (reduce + 0 unmarked)))))

(defn play-number [number board]
  (vec (replace {number nil} board)))

(defn play-board [numbers board]
  (loop [numbers numbers
         board board
         turns 1]
    (if-let [[number & numbers] numbers]
      (let [board (play-number number board)]
        (if-let [score (score-board number board)]
          {:score score :turns turns}
          (recur numbers board (inc turns)))))))

(defn part1 []
  (let [final-states (map #(play-board (:numbers input) %) (:boards input))
        best-board (first (sort-by :turns final-states))]
    (:score best-board)))

(defn part2 []
  (let [final-states (map #(play-board (:numbers input) %) (:boards input))
        worst-board (last (sort-by :turns final-states))]
    (:score worst-board)))

(defn -main []
  (println (part1))
  (println (part2)))

(ns adventofcode2021.day06
  (:use adventofcode2021.advent-util)
  (:require [clojure.string :as str]))

(def input (map parse-int (str/split (read-input) #"[,\s]+")))
;(def input (map parse-int (str/split "3,4,3,1,2" #"[,\s]+")))

(defn step-fish [fish]
  (let [num-completed-fish (count (filter #{0} fish))
        fish (concat (map
                       (fn [f]
                         (case f
                           0 6
                           (dec f)))
                       fish)
                     (repeat num-completed-fish 8))]
    fish))

(defn part1 []
  (count (nth (iterate step-fish input) 80)))

(defn step-fish-part2 [freqs]
  (let [num-spawns (get freqs 0 0)
        freqs (apply merge-with + (map (fn [[k v]]
                                         (let [k (if (= 0 k) 6 (dec k))]
                                           {k v}))
                                       freqs))
        new-spawns {8 num-spawns}]
    (merge-with + freqs new-spawns)))

(defn part2 []
  (let [freqs (frequencies input)]
    (reduce + 0 (vals (nth (iterate step-fish-part2 freqs) 256)))))

(defn -main []
  (println (part1))
  (println (part2)))

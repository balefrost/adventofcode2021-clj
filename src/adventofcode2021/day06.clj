(ns adventofcode2021.day06
  (:use adventofcode2021.advent-util)
  (:require [clojure.string :as str]))

(def input (map parse-int (str/split (read-input) #"[,\s]+")))
;(def input (map parse-int (str/split "3,4,3,1,2" #"[,\s]+")))

(defn step-fish [freqs]
  (let [num-spawns (get freqs 0 0)
        freqs (apply merge-with + (map (fn [[k v]]
                                         (let [k (if (= 0 k) 6 (dec k))]
                                           {k v}))
                                       freqs))
        new-spawns {8 num-spawns}]
    (merge-with + freqs new-spawns)))

(defn solve [days fish]
  (let [freqs (frequencies fish)]
    (reduce + 0 (vals (nth (iterate step-fish freqs) days)))))

(defn part1 []
  (solve 80 input))

(defn part2 []
  (solve 256 input))

(defn -main []
  (println (part1))
  (println (part2)))

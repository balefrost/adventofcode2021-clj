(ns adventofcode2021.day06
  (:use adventofcode2021.advent-util)
  (:require [clojure.string :as str]))

(def input (map parse-int (str/split (read-input) #"[,\s]+")))
;(def input (map parse-int (str/split "3,4,3,1,2" #"[,\s]+")))

(defn decrement-keys [m]
  (into {}
        (map (fn [[k v]] [(dec k) v]) m)))

(defn step-fish [freqs]
  (let [reset-fish {6 (get freqs 0 0)}
        dec-fish (dissoc
                   (decrement-keys freqs)
                   -1)
        spawned-fish {8 (get freqs 0 0)}
        freqs (merge-with + reset-fish dec-fish spawned-fish)]
    freqs))

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

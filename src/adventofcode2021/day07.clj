(ns adventofcode2021.day07
  (:use adventofcode2021.advent-util)
  (:require [clojure.string :as str]))

(def input (map parse-int (str/split (read-input) #"[,\s]+")))
;(def input (map parse-int (str/split "16,1,2,0,4,2,7,1,2,14" #"[,\s]+")))

(defn median [s]
  (let [sorted (into [] (sort s))
        mid-idx (quot (count s) 2)]
    (nth sorted mid-idx)))

(defn part1 []
  (let [med (median input)]
    (reduce + (map (fn [i] (abs-value (- i med))) input))))

(defn cost [dist]
  (int (* (+ 1 dist) (/ dist 2))))

(defn compute-convergence-cost [crabs point]
  (reduce
    +
    (map
      (fn [c]
        (cost (abs-value (- c point))))
      crabs)))

(defn part2 []
  (let [minpos (reduce min input)
        maxpos (reduce max input)]
    (reduce
      min
      (for [conv (range minpos (inc maxpos))]
        (compute-convergence-cost input conv)))))

(defn -main []
  (println (part1))
  (println (part2)))

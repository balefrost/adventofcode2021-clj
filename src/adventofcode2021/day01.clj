(ns adventofcode2021.day01
  (:use adventofcode2021.advent-util))

(def input (parse-input-lines #(parse-int %)))

(defn part1 []
  (count (filter true? (map > (rest input) input))))


(defn part2 []
  (let [threes (partition 3 1 input)
        sums (map (partial reduce + 0) threes)]
    (count (filter true? (map > (rest sums) sums)))))


(defn -main []
  (println (part1))
  (println (part2)))

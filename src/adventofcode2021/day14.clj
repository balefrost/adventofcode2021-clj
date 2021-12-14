(ns adventofcode2021.day14
  (:use adventofcode2021.advent-util)
  (:use clojure.pprint)
  (:require [clojure.string :as str]))

(defn process-input [input]
  (let [lines (str/split-lines input)
        [initial-polymer _ & rest-lines] lines
        initial-polymer (vec initial-polymer)
        rules (into
                {}
                (for [line rest-lines
                      :let [[_ a b to] (re-matches #"(\w)(\w) -> (\w)" line)]]
                  [[(first a) (first b)]
                   (first to)]))]
    {:initial initial-polymer
     :rules   rules}))

(def input (process-input (read-input)))
;(def input (process-input "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C"))

(defn step [rules current]
  (concat
    (take 1 current)
    (mapcat
      (fn [a b]
        (let [to (get rules [a b])]
          [to b]))

      current
      (drop 1 current))))

(defn part1 []
  (let [freqs (frequencies
                (nth
                  (iterate (partial step (:rules input)) (:initial input))
                  10))
        svals (sort (vals freqs))]
    (- (last svals) (first svals))))

(defn step-part2 [mappings counts]
  (let [nc (mapcat
             (fn [[k c]]
               (for [m (get mappings k)]
                 { m c}))
             counts)
        new-counts (reduce
                     (partial merge-with +)
                     {}
                     nc)]
    new-counts))

(defn part2 []
  (let [mappings (into
                   {}
                   (for [[[a b] to] (seq (:rules input))]
                     [[a b] [[a to] [to b]]]))
        pairs (map
                vector
                (:initial input)
                (drop 1 (:initial input)))
        counts (reduce
                 (fn [acc k] (update acc k (fnil inc 0)))
                 {}
                 pairs)
        final-state (nth
                      (iterate (partial step-part2 mappings) counts)
                      40)
        ; all values in final-hist are doubled
        final-hist (reduce
                     (partial merge-with +)
                     {}
                     (concat
                       ; most characters will appear in two pairs. The first and last character each appear in
                       ; just one pair, so add "synthetic" pairs
                       ; perhaps this would be cleaner to include a pair [nil first] and [last nil] with
                       ; no substitution rule.
                       [{(first (:initial input)) 1} {(last (:initial input)) 1}]
                       (mapcat
                         (fn [[[a b] cnt]]
                           [{a cnt}
                            {b cnt}])
                         final-state)))
        svals (sort (vals final-hist))]
    (/ (- (last svals) (first svals)) 2)))


(defn -main []
  (try
    (do
      (println (part1))
      (println (part2)))
    (catch Exception e (.printStackTrace e))))

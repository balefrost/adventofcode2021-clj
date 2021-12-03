(ns adventofcode2021.day03
  (:use adventofcode2021.advent-util)
  (:require [clojure.string :as str]))

(def input (read-input-lines))

(defn part1 []
  (let [best-digits (for [digit-list (apply map vector input)
                          :let [zeros (count (filter #{\0} digit-list))]
                          :let [ones (count (filter #{\1} digit-list))]]
                      (if (> zeros ones)
                        \0
                        \1))
        worst-digits (map {\1 \0 \0 \1} best-digits)
        gamma (parse-int
                (str/join best-digits)
                2)
        epsilon (parse-int
                  (str/join worst-digits)
                  2)]
    (* gamma epsilon)))

(defn most-common-digit [ss idx]
  (let [digits-in-place (map #(nth % idx) ss)
        {zeros \0 ones \1} (frequencies digits-in-place)]
    (if (> zeros ones)
      \0
      \1)))

(defn least-common-digit [ss idx]
  (case (most-common-digit ss idx)
    \0 \1
    \1 \0))

(defn test-candidates [candidates bit-fn]
  (loop [candidates candidates
         idx 0]
    (if (= 1 (count candidates))
      (parse-int (first candidates) 2)
      (let [target-digit (bit-fn candidates idx)
            candidates (filter #(= target-digit (nth % idx)) candidates)]
        (recur candidates (inc idx))))))

(defn part2 []
  (let [oxy (test-candidates input most-common-digit)
        scrubber (test-candidates input least-common-digit)]
    (* oxy scrubber)))

(defn -main []
  (println (part1))
  (println (part2)))

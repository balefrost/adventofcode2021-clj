(ns adventofcode2021.day21
  (:use adventofcode2021.advent-util)
  (:use clojure.pprint)
  (:use [clojure.data.priority-map :only [priority-map]])
  (:require [clojure.string :as str]))

(defn process-input [input-text]
  (let [lines (str/split-lines input-text)
        re #"Player \d starting position: (\d+)"
        [_ p1] (re-matches re (first lines))
        [_ p2] (re-matches re (second lines))
        p1 (parse-int p1)
        p2 (parse-int p2)]
    {:p1 p1
     :p2 p2}))

;(def input (process-input (read-input)))
(def input (process-input "Player 1 starting position: 4\nPlayer 2 starting position: 8"))

(defn do-move [player spaces]
  (let [newpos (inc (rem (+ (dec (:pos player)) spaces) 10))
        newscore (+ (:score player) newpos)]
    {:pos   newpos
     :score newscore}))


(defn do-roll [die]
  [(:next-roll die)
   (-> die
       (update :next-roll #(inc (rem % 100)))
       (update :total-rolls inc))])

(defn part1 []
  (loop [die {:next-roll   1
              :total-rolls 0}
         turn 0
         players [{:pos   (:p1 input)
                   :score 0}
                  {:pos   (:p2 input)
                   :score 0}]]
    (let [[r1 die] (do-roll die)
          [r2 die] (do-roll die)
          [r3 die] (do-roll die)
          sum (+ r1 r2 r3)
          player (get players turn)
          nextturn (rem (inc turn) 2)
          player (do-move player sum)
          players (assoc players turn player)]
      (if (>= (:score player) 1000)
        (* (:total-rolls die) (:score (get players nextturn)))
        (recur die nextturn players)))))

(defn part2 [])

(defn -main []
  (try
    (do
      (println (part1))
      (println (part2)))
    (catch Exception e (.printStackTrace e))))

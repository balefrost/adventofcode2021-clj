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

(def input (process-input (read-input)))
;(def input (process-input "Player 1 starting position: 4\nPlayer 2 starting position: 8"))

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

(def outcome-freqs
  (into {}
        (for [start (range 1 11)]
          [start (frequencies
                   (for [a (range 1 4)
                         b (range 1 4)
                         c (range 1 4)
                         :let [sum (+ a b c)
                               end (inc (rem (+ sum (dec start)) 10))]]
                     end))])))

(defn take-turn [score-freqs]
  (reduce
    #(merge-with + %1 %2)
    {}
    (for [[state state-freq] score-freqs
          :let [{pos :pos score :score} state]
          [new-pos move-freq] (get outcome-freqs pos)]
      {{:pos new-pos :score (+ score new-pos)} (* state-freq move-freq)})))

(defn compute-outcomes-seq [pos]
  (letfn [(helper [freqs outcomes turn-no]
            (lazy-seq
              (cons
                {:freqs freqs :outcomes outcomes :turn-no turn-no}
                (if (not (empty? freqs))
                  (let [new-freqs (take-turn freqs)
                        turn-no (inc turn-no)
                        win-states (filter
                                     (fn [{score :score}]
                                       (>= score 21))
                                     (keys new-freqs))
                        continue-states (filter
                                          (fn [{score :score}]
                                            (< score 21))
                                          (keys new-freqs))
                        win-freq (reduce + 0 (for [k win-states]
                                               (get new-freqs k)))
                        continue-freq (reduce + 0 (for [k continue-states]
                                                    (get new-freqs k)))
                        outcome {:wins      win-freq
                                 :continues continue-freq}
                        outcomes (assoc outcomes turn-no outcome)
                        freqs (reduce dissoc new-freqs win-states)]
                    (helper freqs outcomes turn-no))))))]
    (helper
      {{:pos pos :score 0} 1}
      {0 {:wins 0 :continues 1}}
      0)))

(defn compute-outcomes [pos]
  (:outcomes (last (compute-outcomes-seq pos))))

(defn part2 []
  (let [p1-outcomes (compute-outcomes (:p1 input))
        p2-outcomes (compute-outcomes (:p2 input))
        total-p1-wins (reduce + 0 (for [p1turns (keys p1-outcomes)
                                        :let [p2turns (dec p1turns)
                                              p1wins (get-in p1-outcomes [p1turns :wins])
                                              p2continues (get-in p2-outcomes [p2turns :continues] 0)]]
                                    (* p1wins p2continues)))
        total-p2-wins (reduce + 0 (for [p2turns (keys p2-outcomes)
                                        :let [p1turns p2turns
                                              p2wins (get-in p2-outcomes [p2turns :wins])
                                              p1continues (get-in p1-outcomes [p1turns :continues] 0)]]
                                    (* p2wins p1continues)))]
    (max total-p1-wins total-p2-wins)))

(defn -main []
  (try
    (do
      (println (part1))
      (println (part2)))
    (catch Exception e (.printStackTrace e))))

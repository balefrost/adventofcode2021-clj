(ns adventofcode2021.day02
  (:use adventofcode2021.advent-util))

(defn parse-line [s]
  (let [[_ cmd dist] (re-matches #"(\w+) (\d+)" s)
        dist (parse-int dist)]
    (case cmd
      "forward" [:forward dist]
      "down" [:down dist]
      "up" [:up dist])))

(def input (parse-input-lines #(parse-line %)))

(defn step-part1 [state instr]
  (let [[cmd dist] instr
        diff (case cmd
               :forward [dist 0]
               :down [0 dist]
               :up [0 (- dist)])]
    (mapv + state diff)))

(defn part1 []
  (let [[pos depth] (reduce step-part1 [0 0] input)]
    (* pos depth)))

(defn step-part2 [state instr]
  (let [{aim :aim} state
        [cmd dist] instr]
    (case cmd
      :forward (-> state
                   (update :pos (partial + dist))
                   (update :depth (partial + (* aim dist))))
      :down (update state :aim (partial + dist))
      :up (update state :aim #(- % dist)))))


(defn part2 []
  (let [result (reduce step-part2 {:pos 0 :depth 0 :aim 0} input)]
    (* (:pos result) (:depth result))))

(defn -main []
  (println (part1))
  (println (part2)))

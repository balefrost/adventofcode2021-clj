(ns adventofcode2021.day10
  (:use adventofcode2021.advent-util))

(def input (read-input-lines))
;(def input (str/split-lines "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]"))

(def end-char-mapping {\( \)
                       \[ \]
                       \{ \}
                       \< \>})

(def illegal-char-scores {\) 3
                          \] 57
                          \} 1197
                          \> 25137})

(def completion-char-scores {\) 1
                             \] 2
                             \} 3
                             \> 4})

(defn first-invalid-character [line]
  (loop [line line
         stack []]
    (if-let [[ch & line] (seq line)]
      (let [end-char (get end-char-mapping ch)]
        (cond
          end-char
          (recur line (conj stack end-char))

          (and (not-empty stack) (= ch (last stack)))
          (recur line (pop stack))

          :else
          ch)))))

(defn complete-line [line]
  (loop [line line
         stack []]
    (if-let [[ch & line] (seq line)]
      (let [end-char (get end-char-mapping ch)]
        (cond
          end-char
          (recur line (conj stack end-char))

          (and (not-empty stack) (= ch (last stack)))
          (recur line (pop stack))

          :else
          nil))
      (reverse stack))))




(defn part1 []
  (reduce +
          (->> input
                     (map first-invalid-character)
                     (filter (comp not nil?))
                     (map illegal-char-scores))))

(defn score-completion [completion]
  (reduce
    (fn [acc v] (+ (completion-char-scores v) (* 5 acc)))
    0
    completion))

(defn median [s]
  (let [sorted (into [] (sort s))
        mid-idx (quot (count s) 2)]
    (nth sorted mid-idx)))

; 360491551559 wrong
(defn part2 []
  (median
    (->> input
         (map complete-line)
         (filter (comp not nil?))
         (map score-completion))))


(defn -main []
  (println (part1))
  (println (part2)))

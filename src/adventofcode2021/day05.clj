(ns adventofcode2021.day05
  (:use adventofcode2021.advent-util))

(defn parse-line [line]
  (let [[_ x1 y1 x2 y2] (re-matches #"(\d+),(\d+) -> (\d+),(\d+)" line)]
    {:x1 (parse-int x1)
     :y1 (parse-int y1)
     :x2 (parse-int x2)
     :y2 (parse-int y2)}))

(def input (parse-input-lines parse-line))
;(def input (map parse-line (str/split-lines "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2")))

(defn is-horiz-line [line]
  (let [{:keys [y1 y2]} line]
    (= y1 y2)))

(defn is-vert-line [line]
  (let [{:keys [x1 x2]} line]
    (= x1 x2)))

(defn render-line [board line]
  (let [{:keys [x1 y1 x2 y2]} line]
    (let [xinc (sign (- x2 x1))
          yinc (sign (- y2 y1))
          line-len (inc (max (abs-value (- x2 x1))
                             (abs-value (- y2 y1))))]
      (reduce
        (fn [board i]
          (update board [(+ x1 (* xinc i)) (+ y1 (* yinc i))] (fnil inc 0)))
        board
        (range line-len)))))

(defn part1 []
  (let [relevant-lines (filter (fn [line] (or (is-horiz-line line) (is-vert-line line))) input)
        resulting-board (reduce
                          render-line
                          {}
                          relevant-lines)
        intersections (filter
                        (fn [[_ v]] (> v 1))
                        resulting-board)]
    (count intersections)))


(defn part2 []
  (let [resulting-board (reduce
                          render-line
                          {}
                          input)
        intersections (filter
                        (fn [[_ v]] (> v 1))
                        resulting-board)]
    (count intersections)))

(defn -main []
  (println (part1))
  (println (part2)))

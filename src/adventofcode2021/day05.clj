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
  (let [{:keys [x1 y1 x2 y2]} line]
    (= y1 y2)))

(defn is-vert-line [line]
  (let [{:keys [x1 y1 x2 y2]} line]
    (= x1 x2)))

(defn is-diag-line [line]
  (let [{:keys [x1 y1 x2 y2]} line]
    (= (abs-value (- x2 x1)) (abs-value (- y2 y1)))))

(defn render-line [board line]
  (let [{:keys [x1 y1 x2 y2]} line]
    (cond
      (is-horiz-line line)
      (let [xmin (min x1 x2)
            xmax (max x1 x2)]
        (reduce
          (fn [board x]
            (update board [x y1] (fnil inc 0)))
          board
          (range xmin (inc xmax))))
      (is-vert-line line)
      (let [ymin (min y1 y2)
            ymax (max y1 y2)]
        (reduce
          (fn [board y]
            (update board [x1 y] (fnil inc 0)))
          board
          (range ymin (inc ymax))))
      (is-diag-line line)
      (let [xinc (if (< x1 x2) 1 -1)
            yinc (if (< y1 y2) 1 -1)]
        (reduce
          (fn [board i]
            (update board [(+ x1 (* xinc i)) (+ y1 (* yinc i))] (fnil inc 0)))
          board
          (range (inc (abs-value (- x2 x1)))))))))

(defn part1 []
  (let [relevant-lines (filter (fn [line] (or (is-horiz-line line) (is-vert-line line))) input)
        resulting-board (reduce
                          render-line
                          {}
                          relevant-lines)
        intersections (filter
                        (fn [[k v]] (> v 1))
                        resulting-board)]
    (count intersections)))


(defn part2 []
  (let [resulting-board (reduce
                          render-line
                          {}
                          input)
        intersections (filter
                        (fn [[k v]] (> v 1))
                        resulting-board)]
    (count intersections)))

(defn -main []
  (println (part1))
  (println (part2)))

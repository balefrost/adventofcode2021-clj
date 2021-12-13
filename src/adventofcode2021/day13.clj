(ns adventofcode2021.day13
  (:use adventofcode2021.advent-util)
  (:use clojure.pprint)
  (:require [clojure.string :as str]))

(defn process-input [input]
  (let [lines (str/split-lines input)
        [dot-lines rest-lines] (split-with #(not (str/blank? %)) lines)
        dots (for [line dot-lines
                   :let [[_ xs ys] (re-matches #"(\d+),(\d+)" line)]]
               [(parse-int xs) (parse-int ys)])
        folds (for [line rest-lines
                    :when (not (str/blank? line))
                    :let [[_ xy pos] (re-matches #"fold along ([xy])=(\d+)" line)]]
                {:axis xy
                 :pos  (parse-int pos)})]
    {:dots  (set dots)
     :folds folds}))

(def input (process-input (read-input)))
;(def input (process-input "6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n3,0\n8,4\n1,10\n2,14\n8,10\n9,0\n\nfold along y=7\nfold along x=5"))

(defn do-fold [dots fold]
  (let [{axis :axis pos :pos} fold]
    (set
      (case axis
        "x" (for [dot dots
                  :let [[x y] dot]]
              (if (> x pos)
                [(- pos (- x pos)) y]
                dot))
        "y" (for [dot dots
                  :let [[x y] dot]]
              (if (> y pos)
                [x (- pos (- y pos))]
                dot))))))

(defn part1 []
  (let [{dots :dots folds :folds} input]
    (count (do-fold dots (first folds)))))

(defn display-result [dots]
  (let [xmax (reduce max (map first dots))
        ymax (reduce max (map second dots))]
    (println
      (str/join "\n"
        (for [y (range (inc ymax))]
          (str/join
            (for [x (range (inc xmax))]
              (if (contains? dots [x y])
                "*"
                " "))))))))

(defn part2 []
  (let [{dots :dots folds :folds} input]
    (display-result (reduce (fn [dots fold] (do-fold dots fold)) dots folds))))

(defn -main []
  (try
    (do
      (println (part1))
      (println (part2)))
    (catch Exception e (.printStackTrace e))))

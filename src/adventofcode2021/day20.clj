(ns adventofcode2021.day20
  (:use adventofcode2021.advent-util)
  (:use clojure.pprint)
  (:use [clojure.data.priority-map :only [priority-map]])
  (:require [clojure.string :as str]))

(defn process-input [input-text]
  (let [[lookup _ & image] (str/split-lines input-text)
        lookup (mapv
                 (fn [ch]
                   (case ch
                     \# 1
                     0))
                 lookup)
        pixels (into
                 {}
                 (for [[lidx line] (zipmap (range) image)
                       [cidx ch] (zipmap (range) line)]
                   [{:x cidx :y lidx} (if (= ch \#) 1 0)]))]
    {:lookup lookup
     :image  {:back   0
              :pixels pixels
              :xmin 0
              :xmax (reduce max (map count image))
              :ymin 0
              :ymax (count image)}}))

(def input (process-input (read-input)))
;(def input (process-input "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#\n\n#..#.\n#....\n##..#\n..#..\n..###"))
;(def input (process-input "#.#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#\n\n#..#.\n#....\n##..#\n..#..\n..##."))

(defn kernel-coords [x y]
  (for [y (range (dec y) (+ 2 y))
        x (range (dec x) (+ 2 x))]
    {:x x :y y}))

(defn step-image [lookup image]
  (let [xmin (dec (:xmin image))
        xmax (inc (:xmax image))
        ymin (dec (:ymin image))
        ymax (inc (:ymax image))
        pixels (into {}
                     (for [x (range xmin xmax)
                           y (range ymin ymax)
                           :let [c (kernel-coords x y)
                                 bits (map #(get (:pixels image) % (:back image)) c)
                                 idx (parse-int (str/join bits) 2)
                                 newval (get lookup idx :error)]]
                       [{:x x :y y} newval]))]
    {:back   (case (:back image)
               0 (get lookup 0)
               1 (get lookup 511))
     :pixels pixels
     :xmin xmin
     :xmax xmax
     :ymin ymin
     :ymax ymax}))

(defn visualize [image]
  (letfn [(convert-digit [digit] (case digit 0 \. \#))]
    (str/join
      "\n"
      (for [y (range (dec (:ymin image)) (inc (:ymax image)))]
        (str/join
          (for [x (range (dec (:xmin image)) (inc (:xmax image)))]
            (let [val (get (:pixels image) {:x x :y y} (:back image))]
              (convert-digit val))))))))

(defn part1 []
  (let [{lookup :lookup image :image} input
        final (nth (iterate #(step-image lookup %) image) 2)]
    (count (filter #{1} (vals (:pixels final))))))

(defn part2 []
  (let [{lookup :lookup image :image} input
        final (nth (iterate #(step-image lookup %) image) 50)]
    (count (filter #{1} (vals (:pixels final))))))

(defn -main []
  (try
    (do
      (println (part1))
      (println (part2)))
    (catch Exception e (.printStackTrace e))))

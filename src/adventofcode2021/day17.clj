(ns adventofcode2021.day17
  (:use adventofcode2021.advent-util)
  (:use clojure.pprint)
  (:use [clojure.data.priority-map :only [priority-map]])
  (:require [clojure.string :as str]))

(defn parse-input [line]
  (let [[_ x1 x2 y1 y2] (re-matches #"target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)" line)
        x1 (parse-int x1)
        x2 (parse-int x2)
        y1 (parse-int y1)
        y2 (parse-int y2)]
    {:x1 (min x1 x2)
     :x2 (max x1 x2)
     :y1 (min y1 y2)
     :y2 (max y1 y2)}))

(defn step-state [state]
  (let [{:keys [x y vx vy]} state
        x (+ x vx)
        y (+ y vy)
        vx (cond
             (> vx 0) (dec vx)
             (< vx 0) (inc vx)
             :else vx)
        vy (dec vy)]
    {:x  x
     :y  y
     :vx vx
     :vy vy}))

(defn test-position [target state]
  (let [{:keys [x1 x2 y1 y2]} target
        {:keys [x y]} state]
    (and
      (<= x1 x x2)
      (<= y1 y y2))))

(defn generate-states [state]
  (iterate step-state state))

(defn clip-arc [target arc]
  (take-while
    (fn [{y :y}] (>= y (min (:y1 target) (:y2 target))))
    arc))

(defn test-arc [target init-state]
  (some #(test-position target %) (clip-arc target (generate-states init-state))))

(def input (parse-input (str/trim (read-input))))
;(def input (parse-input "target area: x=20..30, y=-10..-5"))

(defn part1 []
  (let [{:keys [x1 x2 y1 y2]} input
        lowvy (- (min y1 y2))
        highvy (min y1 y2)
        minvy (min lowvy highvy)
        maxvy (max lowvy highvy)
        hits (for [vy (range minvy (inc maxvy))
                   vx (range (inc (max x1 x2)))
                   :let [init-state {:x 0 :y 0 :vx vx :vy vy}]
                   :when (test-arc input init-state)]
               init-state)
        altitudes (for [hit hits
                        :let [arc (clip-arc input (generate-states hit))]]
                    (reduce
                      max
                      (for [pos arc]
                        (:y pos))))]
    (reduce max altitudes)))

(defn part2 []
  (let [{:keys [x1 x2 y1 y2]} input
        lowvy (- (min y1 y2))
        highvy (min y1 y2)
        minvy (min lowvy highvy)
        maxvy (max lowvy highvy)
        hits (for [vy (range minvy (inc maxvy))
                   vx (range (inc (max x1 x2)))
                   :let [init-state {:x 0 :y 0 :vx vx :vy vy}]
                   :when (test-arc input init-state)]
               init-state)]
    (count hits)))

(defn -main []
  (try
    (do
      (println (part1))
      (println (part2)))
    (catch Exception e (.printStackTrace e))))

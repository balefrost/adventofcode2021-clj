(ns adventofcode2021.day25
  (:use adventofcode2021.advent-util)
  (:use clojure.pprint)
  (:use [clojure.data.priority-map :only [priority-map]])
  (:require [clojure.string :as str]))

(defn process-input [input-str]
  (let [lines (str/split-lines input-str)
        pawns (into {} (for [[line y] (map vector lines (range))
                             [ch x] (map vector line (range))
                             :when (#{\> \v} ch)]
                         [{:x x :y y}
                          ch]))
        dims {:h (count lines) :w (count (first lines))}]
    {:pawns pawns
     :dims  dims}))

(def input
  (process-input (read-input)))

(defn next-cell [dims pos ch]
  (let [{:keys [y x]} pos
        {:keys [h w]} dims
        [vx vy] (case ch
                  \> [1 0]
                  \v [0 1])
        y (rem (+ y vy) h)
        x (rem (+ x vx) w)]
    {:y y :x x}))

(defn format-pawns [dims pawns]
  (let [{w :w h :h} dims]
    (str/join
      "\n"
      (for [y (range h)]
        (str/join
          (for [x (range w)]
            (get pawns {:y y :x x} \.)))))))

(defn try-move-pawns [dims pawns ch]
  (let [to-move (filter (fn [[_ c]] (= c ch)) pawns)]
    (reduce
      (fn [acc [pos ch]]
        (let [newpos (next-cell dims pos ch)]
          (if (not (contains? pawns newpos))
            (-> acc
                (dissoc pos)
                (assoc newpos ch))
            acc)))
      pawns
      to-move)))

;pawns (reduce
;        (fn [[pos ch]]
;          (let [newpos (next-cell dims pos ch)]
;            (if (not (contains? pawns newpos))
;              (-> pawns
;                  (dissoc pos)
;                  (assoc newpos ch)))))
;        pawns
;        rights)


(defn step-state [dims pawns]
  (let [pawns (try-move-pawns dims pawns \>)
        pawns (try-move-pawns dims pawns \v)]
    pawns))



;(def input (process-input "...>...\n.......\n......>\nv.....>\n......>\n.......\n..vvv.."))
;(def input (process-input "v...>>.vv>\n.vv>>.vv..\n>>.>v>...v\n>>v>>.>.v.\nv>v.vv.v..\n>.>>..v...\n.vv..>.>v.\nv.v..>>v.v\n....v..v.>"))

(defn part1 []
  (let [{:keys [dims pawns]} input
        [_ steps] (loop [current-pawns pawns
                         steps 0]
                    (let [new-pawns (step-state dims current-pawns)]
                      (if (= current-pawns new-pawns)
                        [current-pawns steps]
                        (recur new-pawns (inc steps)))))]
    (inc steps)))

(defn part2 [])

(defn -main []
  (try
    (do
      (println (part1))
      (println (part2)))
    (catch Exception e (.printStackTrace e))))

(ns adventofcode2021.day18
  (:use adventofcode2021.advent-util)
  (:use clojure.pprint)
  (:use [clojure.data.priority-map :only [priority-map]])
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (vec
    (lazy-seq
      (loop [line line]
        (if-let [[ch & restline] (seq line)]
          (case ch
            \[ (cons :enter (parse-line restline))
            \] (cons :exit (parse-line restline))
            \, (recur restline)
            (let [[digits restline] (split-with #(Character/isDigit ^char %) line)]
              (cons (parse-int (str/join digits)) (parse-line restline)))))))))

(def input (parse-input-lines parse-line))

(defn index-of
  ([pred v]
   (index-of pred v 0))
  ([pred v sindex]
   (index-of pred v sindex (count v)))
  ([pred v sindex eindex]
   (loop [s (subvec v sindex eindex)
          idx sindex]
     (if-let [[item & s] s]
       (if (pred item)
         idx
         (recur s (inc idx)))))))

(defn last-index-of
  ([pred v]
   (last-index-of pred v 0))
  ([pred v sindex]
   (last-index-of pred v sindex (count v)))
  ([pred v sindex eindex]
   (loop [s (rseq (subvec v sindex eindex))
          idx (dec eindex)]
     (if-let [[item & s] s]
       (if (pred item)
         idx
         (recur s (dec idx)))))))

(defn splice [v idx nremove & items]
  (let [pre (subvec v 0 idx)
        post (subvec v (+ idx nremove))]
    (vec (concat pre items post))))

(defn explode-sf [sfvec]
  (loop [idx 0
         depth 0]
    (if (< idx (count sfvec))
      (let [a (nth sfvec idx)
            b (nth sfvec (inc idx) nil)]
        (cond
          (= a :enter)
          (recur (inc idx) (inc depth))

          (= a :exit)
          (recur (inc idx) (dec depth))

          (< depth 5)
          (recur (inc idx) depth)

          (and (number? a) (number? b))
          (do
            (let [prevnumidx (last-index-of number? sfvec 0 idx)
                  nextnumidx (index-of number? sfvec (+ 2 idx))
                  sfvec (if prevnumidx
                          (update sfvec prevnumidx + a)
                          sfvec)
                  sfvec (if nextnumidx
                          (update sfvec nextnumidx + b)
                          sfvec)
                  sfvec (splice sfvec (dec idx) 4 0)]
              sfvec))))
      nil)))

(defn split-sf [sfvec]
  (loop [idx 0]
    (if (< idx (count sfvec))
      (let [a (nth sfvec idx)]
        (if (and (number? a) (> a 9))
          (let [half (quot a 2)]
            (splice sfvec idx 1 :enter half (- a half) :exit))
          (recur (inc idx))))
      nil)))

(defn reduce-sf [sfvec]
  (if-let [sfvec (explode-sf sfvec)]
    (recur sfvec)
    (if-let [sfvec (split-sf sfvec)]
      (recur sfvec)
      sfvec)))

(defn add-sf [a b]
  (reduce-sf (vec (concat [:enter] a b [:exit]))))

; Computes the magnitude using a reverse-polish approach. In a valid SF number,
; every :exit we encounter is the end of a pair; moreover, everything on the stack
; at that point represents an already-simplified number.
(defn magnitude-sf [sfvec]
  (loop [sfvec sfvec
         stack []]
    (if-let [[item & sfvec] sfvec]
      (case item
        :enter (recur sfvec stack)
        :exit (let [a (nth stack (- (count stack) 2))
                    b (nth stack (- (count stack) 1))
                    stack (subvec stack 0 (- (count stack) 2))
                    stack (conj stack (+ (* 3 a) (* 2 b)))]
                (recur sfvec stack))
        (recur sfvec (conj stack item)))
      (first stack))))


(defn format-sf [sfvec]
  (str/join
    (mapcat
      (fn [a b]
        (let [astr (case a
                     :enter \[
                     :exit \]
                     a)]
          (cond
            (= a :enter) [astr]
            (= b :exit) [astr]
            :else [astr, \,])))
      sfvec
      (concat (drop 1 sfvec) [:exit]))))

(defn part1 []
  (let [final (reduce add-sf input)]
    (magnitude-sf final)))


(defn part2 []
  (reduce
    max
    (for [a input
          b input]
      (magnitude-sf (add-sf a b)))))

(defn -main []
  (try
    (do
      (println (part1))
      (println (part2)))
    (catch Exception e (.printStackTrace e))))

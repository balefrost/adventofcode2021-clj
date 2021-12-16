(ns adventofcode2021.day16
  (:use adventofcode2021.advent-util)
  (:use clojure.pprint)
  (:use [clojure.data.priority-map :only [priority-map]])
  (:require [clojure.string :as str]))

(def input (str/trim (read-input)))
;(def input "CE00C43D881120")
;(def input "38006F45291200")
;(def input "A0016C880162017C3686B18A3D4780")

(defn bits [num w]
  (lazy-seq
    (if (= 0 w)
      nil
      (cons (bit-and 1 (bit-shift-right num (dec w))) (bits num (dec w))))))


(defn process-input [input]
  (for [ch input
        :let [val (parse-int (str ch) 16)
              bs (bits val 4)]
        b bs]
    b))

(defn parse-bits [bits]
  (reduce
    (fn [acc b]
      (+ b (* 2 acc)))
    0
    bits))

(defn split-at-or-throw [n s]
  (let [[hd :as result] (split-at n s)]
    (assert (= n (count hd)) "not enough items left")
    result))

(defn parse-next-n-bits [n bits]
  (let [[hd tl] (split-at-or-throw n bits)]
    [(parse-bits hd) tl]))

(defn parse-literal-packet [bits]
  (loop [digits []
         bits bits]
    (let [[b & bits] bits
          last (= b 0)
          [val-bits bits] (split-at-or-throw 4 bits)
          digits (into digits val-bits)]
      (if last
        [(parse-bits digits) bits]
        (recur digits bits)))))

(def parse-single-packet)

(defn parse-packet-list [bits]
  (loop [bits bits
         result []]
    (if (seq bits)
      (let [[packet bits] (parse-single-packet bits)
            result (conj result packet)]
        (recur bits result))
      result)))


(defn parse-operator-packet [bits]
  (let [[b & bits] bits]
    (case b
      0 (let [[bitlength bits] (parse-next-n-bits 15 bits)
              [subpackets-bits bits] (split-at-or-throw bitlength bits)
              subpackets (parse-packet-list subpackets-bits)]
          [subpackets bits])
      1 (let [[nsubs bits] (parse-next-n-bits 11 bits)]
          (loop [nsubs nsubs
                 bits bits
                 result []]
            (if (= 0 nsubs)
              [result bits]
              (let [[subpacket bits] (parse-single-packet bits)
                    nsubs (dec nsubs)
                    result (conj result subpacket)]
                (recur nsubs bits result))))))))


(defn parse-single-packet [bits]
  (let [[version bits] (parse-next-n-bits 3 bits)
        [type-id bits] (parse-next-n-bits 3 bits)
        result {:version version :type-id type-id}]
    (case type-id
      4 (let [[packet bits] (parse-literal-packet bits)]
          [(merge result {:literal packet}) bits])
      (let [[packet bits] (parse-operator-packet bits)]
        [(merge result {:children packet}) bits]))))

(defn walk-packets [packet]
  (cons (dissoc packet :children) (for [child (:children packet)
                                        r (walk-packets child)]
                                    r)))

(defn part1 []
  (let [bits (process-input input)
        [top-packet _] (parse-single-packet bits)
        packets (walk-packets top-packet)
        versions (map :version packets)]
    (reduce
      +
      0
      versions)))

(defn evaluate-packet [packet]
  (let [evaluated-children (map evaluate-packet (:children packet))]
    (case (:type-id packet)
      0 (reduce + 0 evaluated-children)
      1 (reduce * 1 evaluated-children)
      2 (reduce min evaluated-children)
      3 (reduce max evaluated-children)
      4 (:literal packet)
      5 (let [[a b] evaluated-children]
          (if (> a b) 1 0))
      6 (let [[a b] evaluated-children]
          (if (< a b) 1 0))
      7 (let [[a b] evaluated-children]
          (if (= a b) 1 0)))))

(defn part2 []
  (let [bits (process-input input)
        [top-packet _] (parse-single-packet bits)]
    (evaluate-packet top-packet)))

(defn -main []
  (try
    (do
      (println (part1))
      (println (part2)))
    (catch Exception e (.printStackTrace e))))

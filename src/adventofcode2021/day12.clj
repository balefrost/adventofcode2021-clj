(ns adventofcode2021.day12
  (:use adventofcode2021.advent-util)
  (:use clojure.pprint)
  (:require [clojure.string :as str]))

(defn process-input [input]
  (map
    (fn [line]
      (let [[_ from to] (re-matches #"(\w+)-(\w+)" line)]
        [from to]))
    (str/split-lines input)))

(def input (process-input (read-input)))
;(def input (process-input "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end"))

(defn mirror-edges [edges]
  (into #{} (filter
              (fn [[from to]]
                (and (not (= to "start"))
                     (not (= from "end"))))
              (mapcat
                (fn [[from to]]
                  [[from to] [to from]])
                edges))))

(defn is-small [name]
  (<= (int \a) (int (.charAt name 0)) (int \z)))

(defn is-big [name]
  (<= (int \A) (int (.charAt name 0)) (int \Z)))

(defn find-paths [edges]
  (let [with-mirrored (mirror-edges edges)]
    (letfn [(helper [worklist]
              (lazy-seq
                (if-let [[work-item & worklist] (seq worklist)]
                  (let [{path :path current :current visited :visited} work-item]
                    (if (= current "end")
                      (cons path (helper worklist))
                      (let [targets (for [[from to] with-mirrored
                                          :when (= from current)
                                          :when (or (is-big to)
                                                    (not (contains? visited to)))]
                                      to)
                            new-work-items (for [t targets]
                                             {:path (conj path t)
                                              :current t
                                              :visited (conj visited t)})]
                        (helper (concat new-work-items worklist))))))))]
      (helper [{:path ["start"] :current "start" :visited #{"start"}}]))))


(defn part1 []
  (count (find-paths input)))

(defn transition-to-vertex [state to]
  (let [{path :path current :current visited :visited sc-spare :sc-spare} state]
    (if (or (is-big to)
            sc-spare
            (not (visited to)))
      (let [spare-consumed (and (is-small to) (visited to))]
        {:path (conj path to)
         :current to
         :visited (conj visited to)
         :sc-spare (if spare-consumed false sc-spare)}))))


(defn find-paths-part2 [edges]
  (let [with-mirrored (mirror-edges edges)]
    (letfn [(helper [worklist]
              (lazy-seq
                ;(pprint worklist)
                (if-let [[work-item & worklist] (seq worklist)]
                  (let [{path :path current :current visited :visited sc-spare :sc-spare} work-item]
                    (if (= current "end")
                      (cons path (helper worklist))
                      (let [new-work-items (for [[from to] with-mirrored
                                                 :when (= from current)
                                                 :let [new-item (transition-to-vertex work-item to)]
                                                 :when (not (nil? new-item))]
                                             new-item)]

                        (helper (concat new-work-items worklist))))))))]
      (helper [{:path ["start"] :current "start" :visited #{"start"} :sc-spare true}]))))


(defn part2 []
  (count (find-paths-part2 input)))

(defn -main []
  (println (part1))
  (println (part2)))

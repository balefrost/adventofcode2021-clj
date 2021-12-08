(ns adventofcode2021.day08
  (:use adventofcode2021.advent-util)
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (let [[input output] (str/split line #" \| ")
        input (re-seq #"\w+" input)
        output (re-seq #"\w+" output)]
    [input output]))

(def input (parse-input-lines parse-line))

(defn part1 []
  (count (filter #{2 3 4 7} (map count (mapcat second input)))))

(def all-signals #{\a \b \c \d \e \f \g})

(defn all-substitutions [options]
  (letfn [(helper [work-list]
            (lazy-seq
              (loop [work-list work-list]
                (if-let [[[remaining-from remaining-to subs-so-far] & work-list] (seq work-list)]
                  (if (empty? remaining-from)
                    (cons subs-so-far (helper work-list))
                    (let [[f & remaining-from] remaining-from
                          new-items (for [t remaining-to]
                                      [remaining-from (disj remaining-to t) (assoc subs-so-far f t)])]
                      (recur (concat new-items work-list))))))))]
    (helper [[options options {}]])))


(def signal-presence
  {0 #{\a \b \c \e \f \g}
   1 #{\c \f}
   2 #{\a \c \d \e \g}
   3 #{\a \c \d \f \g}
   4 #{\b \c \d \f}
   5 #{\a \b \d \f \g}
   6 #{\a \b \d \e \f \g}
   7 #{\a \c \f}
   8 #{\a \b \c \d \e \f \g}
   9 #{\a \b \c \d \f \g}})

(def inv-signal-presence
  (into {} (map (fn [[k v]] [v k]) signal-presence)))

(def valid-signal-combinations (set (vals signal-presence)))

(defn test-individual-digit [subst digit]
  (contains? valid-signal-combinations (into #{} (map subst digit))))

(defn test-substitution [subst inputs outputs]
  (let [all-digits (concat inputs outputs)]
    (if (every? #(test-individual-digit subst %) all-digits)
      subst)))


(defn part2 []
  (reduce +
    (for [[inputs outputs] input
          subst (all-substitutions all-signals)
          :when (test-substitution subst inputs outputs)]
        (parse-int
          (str/join (for [od outputs]
                      (get inv-signal-presence (into #{} (map subst od)))))
          10))))


(defn -main []
  (println (part1))
  (println (part2)))

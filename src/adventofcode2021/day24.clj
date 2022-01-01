(ns adventofcode2021.day24
  (:use adventofcode2021.advent-util)
  (:use clojure.pprint)
  (:use [clojure.data.priority-map :only [priority-map]])
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (let [[op & args] (str/split line #" ")]
    {:op   op
     :args (vec (for [a args]
                  (if (re-matches #"-?\d+" a)
                    (parse-int a)
                    a)))}))

(def input (vec (parse-input-lines parse-line)))

(defn make-register [name idx]
  {:reg name :idx idx})

(defn is-register? [value]
  (and
    (map? value)
    (contains? value :reg)))

(defn is-expr? [value]
  (and
    (map? value)
    (contains? value :op)))

(defn reformat-value [value]
  (cond
    (number? value)
    (str value)

    (string? value)
    value

    (is-register? value)
    (format "%s%d" (:reg value) (:idx value))

    (is-expr? value)
    (let [{:keys [op args]} value
          [a1 a2] (map reformat-value args)]
      (case op
        "inp" (format "(input %s)" a1)
        "add" (format "%s + %s" a1 a2)
        "mul" (format "%s * %s" a1 a2)
        "div" (format "%s / %s" a1 a2)
        "mod" (format "%s %% %s" a1 a2)
        "eql" (format "%s == %s ? 1 : 0" a1 a2)))

    (nil? value)
    nil

    :else
    (assert false (str "no matching clause: " value))))

(defn format-assign-pretty [assign]
  (let [{dest :dest value :value} assign
        deststr (reformat-value dest)
        valuestr (reformat-value value)]
    (format "%s <- %s" deststr valuestr)))

(defn format-assign [assign]
  (let [s (format-assign-pretty assign)]
    (format "%-30s %s" s assign)))

(defn print-assigns [assigns]
  (doseq [a assigns]
    (println (format-assign a))))

(defn update-args [args current-idxs]
  (vec
    (for [arg args]
      (if (string? arg)
        (make-register arg (get current-idxs arg 0))
        arg))))

(defn uniquify-registers [assigns]
  (loop [assigns assigns
         current-idxs {}
         result []]
    (if-let [[assign & assigns] assigns]
      (let [{:keys [dest value]} assign
            value (cond
                    (number? value)
                    value

                    (string? value)
                    (make-register value (get current-idxs value 0))

                    (is-register? value)
                    value

                    (is-expr? value)
                    (update value :args update-args current-idxs)

                    :else
                    (assert false (str "no matching clause: " value)))
            current-idxs (update current-idxs dest (fnil inc 0))
            destidx (get current-idxs dest)
            op {:dest  (make-register dest destidx)
                :value value}]
        (recur assigns current-idxs (conj result op)))
      result)))

(defn replace-registers-in-assign [assign replacement]
  (let [{value :value} assign]
    (let [newvalue (cond
                     (is-register? value)
                     (get replacement value value)

                     (is-expr? value)
                     (let [{args :args} value
                           args (map #(get replacement % %) args)]
                       (assoc value :args args))

                     :else value)]
      (assoc assign :value newvalue))))

; ranges are inclusive. An empty range is represented as nil.
(defn range-has-single-value? [range]
  (and (= 2 (count range))
       (let [[a b] range]
         (= a b))))

(defn find-value-range [value register-ranges]
  (cond
    (number? value)
    [value [value value]]

    (is-register? value)
    (let [rng (get register-ranges value)]
      (if (range-has-single-value? rng)
        [(first rng) rng]
        [value rng]))

    (is-expr? value)
    (let [args (:args value)
          [[value1 rng1] [value2 rng2]] (map #(find-value-range % register-ranges) args)
          values [value1 value2]
          rngs [rng1 rng2]
          updated-value (assoc value :args values)]
      (case (:op value)
        "add" (cond
                (= 0 value1)
                [value2 rng2]

                (= 0 value2)
                [value1 rng1]

                (not-any? nil? rngs)
                [updated-value
                 [(+ (first rng1) (first rng2)) (+ (second rng1) (second rng2))]]

                :else
                [updated-value
                 nil])
        "mul" (cond
                (= 0 value1)
                [0 [0 0]]

                (= 0 value2)
                [0 [0 0]]

                (= 1 value1)
                [value2 rng2]

                (= 1 value2)
                [value1 rng1]

                (not-any? nil? rngs)
                [updated-value
                 [(* (first rng1) (first rng2)) (* (second rng1) (second rng2))]]

                :else
                [updated-value
                 nil])
        "div" (cond
                (= 0 value1)
                [0 [0 0]]

                (= 1 value2)
                [value1 rng1]

                (not-any? nil? rngs)
                [updated-value
                 [(quot (first rng1) (second rng2)) (quot (second rng1) (first rng2))]]

                :else
                [updated-value
                 nil])
        "mod" (cond
                (every? number? values)
                (let [r (rem value1 value2)]
                  [r [r r]])

                (= 0 value1)
                [0 [0 0]]

                (and (not (nil? rng1))
                     (number? value2)
                     (>= (first rng1) 0)
                     (< (second rng1) value2))
                [value1 rng1]

                rng2
                [updated-value
                 [0 (dec (second rng2))]]

                :else
                [updated-value
                 nil])
        "eql" (cond
                (every? number? values)
                (if (= value1 value2)
                  [1 [1 1]]
                  [0 [0 0]])

                (and
                  (not (nil? rng1))
                  (not (nil? rng2))
                  (let [low (max (first rng1) (first rng2))
                        high (min (second rng1) (second rng2))]
                    (< high low)))
                [0 [0 0]]

                (and
                  (not (nil? rng1))
                  (number? value2)
                  (not (<= (first rng1) value2 (second rng1))))
                [0 [0 0]]

                (and
                  (not (nil? rng2))
                  (number? value1)
                  (not (<= (first rng2) value1 (second rng2))))
                [0 [0 0]]

                :else
                [updated-value
                 [0 1]])))

    :else
    (assert false (str "no matching clause: " value))))

(defn forward-register-assignments [assigns]
  (letfn [(helper [assigns replacements]
            (lazy-seq
              (if-let [[assign & assigns] assigns]
                (let [assign (replace-registers-in-assign assign replacements)
                      replacements (if (is-register? (:value assign))
                                     (assoc replacements (:dest assign) (:value assign))
                                     replacements)]
                  (cons assign (helper assigns replacements))))))]
    (helper assigns {})))

(defn coalesce-additions [assigns]
  (letfn [(sort-addition-args [args]
            (concat (filter is-register? args) (filter number? args)))
          (helper [assigns adds]
            (lazy-seq
              (if-let [[assign & assigns] assigns]
                (let [
                      value (:value assign)
                      candidate-value (if (and (is-expr? value) (= "add" (:op value)))
                                        (let [[arg1 arg2] (sort-addition-args (:args value))]
                                          (if (and (is-register? arg1) (number? arg2))
                                            (if-let [[reg const] (get adds arg1)]
                                              {:op "add" :args [reg (+ const arg2)]}))))
                      new-value (if (nil? candidate-value)
                                  value
                                  candidate-value)
                      new-adds (if (and (is-expr? value) (= "add" (:op value)))
                                 (let [[arg1 arg2] (sort-addition-args (:args value))]
                                   (if (and (is-register? arg1) (number? arg2))
                                     (assoc adds (:dest assign) [arg1 arg2])
                                     adds)))

                      new-assign (assoc assign :value new-value)]
                  (cons new-assign (helper assigns new-adds))))))]
    (helper assigns {})))

(defn gather-input-registers [assigns]
  (for [a assigns
        reg (let [v (:value a)]
              (cond
                (is-register? v)
                [v]

                (is-expr? v)
                (for [a (:args v)
                      :when (is-register? a)]
                  a)

                :else
                []))
        :when (= "in" (:reg reg))]
    reg))

(defn analyze-values [assigns]
  (let [input-registers (gather-input-registers assigns)
        register-ranges (into {} (for [r input-registers] [r [1 9]]))]
    (letfn [(helper [assigns register-ranges]
              (lazy-seq
                (if-let [[assign & assigns] assigns]
                  (let [value (:value assign)
                        [simplified-value range] (find-value-range value register-ranges)
                        updated-assign (assoc assign :value simplified-value)
                        register-ranges (assoc register-ranges (:dest updated-assign) range)]
                    ;(println (format "%-30s  %20s  %s%s" (format-assign-pretty updated-assign) range "was " (format-assign-pretty assign)))
                    ;(println (format "  %s" (format-assign assign)))
                    ;(println (format "  %s" (format-assign updated-assign)))
                    ;(doseq [reg (sort-by (juxt :reg :idx) (keys register-ranges))]
                    ;  (let [range (get register-ranges reg)]
                    ;    (println (format "      %6s%-3d : %s" (:reg reg) (:idx reg) range))))
                    (cons updated-assign (helper assigns register-ranges))))))]
      (doall (helper assigns register-ranges)))))

(defn remove-unused-assignments [used-registers assigns]
  (last
    (iterate-until-stable
      (fn [assigns]
        (let [used-registers (into used-registers (for [a assigns
                                                        :let [value (:value a)]
                                                        r (cond
                                                            (number? value)
                                                            []

                                                            (is-register? value)
                                                            [value]

                                                            (is-expr? value)
                                                            (:args value)

                                                            :else
                                                            (assert false (str "no matching clause: " value)))
                                                        :when (is-register? r)]
                                                    r))]
          (for [a assigns
                :when (contains? used-registers (:dest a))]
            a)))
      assigns)))

(defn run-optimizations [optimizations assigns]
  (reduce
    (fn [assigns [_ opt-fn]]
      (let [assigns (doall (opt-fn assigns))]
        ;(println "--------")
        ;(println name)
        ;(print-assigns assigns)
        ;(println "--------")
        assigns))
    assigns
    optimizations))

(defn make-ssa [input]
  (letfn [(helper [input next-input-r]
            (lazy-seq
              (if-let [[instr & input] input]
                (let [op (:op instr)
                      [value next-input-r] (if (= "inp" op)
                                             [(make-register "in" next-input-r) (inc next-input-r)]
                                             [instr next-input-r])

                      r {:dest  (get-in instr [:args 0])
                         :value value}]
                  (cons r (helper input next-input-r))))))]

    (concat
      [{:dest "w" :value 0}
       {:dest "x" :value 0}
       {:dest "y" :value 0}
       {:dest "z" :value 0}]
      (helper input 0))))

(defn find-last-assignment [assigns reg]
  (last (for [a assigns
              :let [dest (:dest a)]
              :when (= reg (:reg dest))]
          dest)))

(defn optimize [input]
  (let [assigns (make-ssa input)
        ;_ (println "--------")
        ;_ (println "assigns")
        ;_ (print-assigns assigns)
        ;_ (println "--------")

        assigns (uniquify-registers assigns)

        ;_ (println "--------")
        ;_ (println "unique")
        ;_ (print-assigns assigns)
        ;_ (println "--------")

        assigns (last (iterate-until-stable
                        (fn [assigns]
                          (run-optimizations
                            [
                             ["values analyzed" analyze-values]
                             ["register assignments forwarded" forward-register-assignments]
                             ["additions coalesced" coalesce-additions]
                             ["unused removed" (fn [assigns]
                                                 (let [last-z (find-last-assignment assigns "z")]
                                                   (remove-unused-assignments #{last-z} assigns)))]]
                            assigns))
                        assigns))]
    assigns))

(defn find-value-ranges [assigns register-ranges]
  (reduce
    (fn [register-ranges assign]
      (let [value (:value assign)
            [_ range] (find-value-range value register-ranges)
            register-ranges (assoc register-ranges (:dest assign) range)]
        register-ranges))
    register-ranges
    assigns))

(defn solve-digits [assigns num-digits digit-seq]
  (let [last-z (find-last-assignment assigns "z")
        input-registers (gather-input-registers assigns)
        register-ranges (into {} (for [r input-registers] [r [0 9]]))]
    (letfn [(helper [digits-so-far]
              (if (= num-digits (count digits-so-far))
                [digits-so-far]
                (do
                  (let [input-reg-name {:reg "in" :idx (count digits-so-far)}
                        register-ranges (reduce (fn [acc [digit idx]]
                                                  (assoc acc {:reg "in" :idx idx} [digit digit]))
                                                register-ranges
                                                (map vector digits-so-far (range)))]
                    (take 1 (for [digit digit-seq
                                  :let [register-ranges (assoc register-ranges input-reg-name [digit digit])
                                        this-digit-ranges (find-value-ranges assigns register-ranges)
                                        [low high] (get this-digit-ranges last-z)]
                                  :when (<= low 0 high)
                                  solution (helper (conj digits-so-far digit))]
                              solution))))))]
      (first (helper [])))))


(defn solve-nth-digit [assigns n digits-so-far]
  (let [last-z (find-last-assignment assigns "z")
        input-registers (gather-input-registers assigns)
        register-ranges (into {} (for [r input-registers] [r [0 9]]))
        register-ranges (reduce (fn [acc [digit idx]]
                                  (assoc acc {:reg "in" :idx idx} [digit digit]))
                                register-ranges
                                (map vector digits-so-far (range)))]
    (first (for [d (range 9 -1 -1)
                 :let [register-ranges (assoc register-ranges {:reg "in" :idx n} [d d])
                       register-ranges (reduce
                                         (fn [register-ranges assign]
                                           (let [value (:value assign)
                                                 [_ range] (find-value-range value register-ranges)
                                                 register-ranges (assoc register-ranges (:dest assign) range)]
                                             register-ranges))
                                         register-ranges
                                         assigns)
                       [low high] (get register-ranges last-z)]
                 :when (<= low 0 high)]
             d))))

(defn run-program [program input]
  (letfn [(evaluate-value [registers value]
            (cond
              (number? value)
              value

              (is-register? value)
              (get registers value)

              (is-expr? value)
              (let [[arg1 arg2] (map #(evaluate-value registers %) (:args value))]
                (case (:op value)
                  "add" (+ arg1 arg2)
                  "mul" (* arg1 arg2)
                  "div" (quot arg1 arg2)
                  "mod" (rem arg1 arg2)
                  "eql" (if (= arg1 arg2) 1 0)))

              :else
              (assert false (format "invalid form %s" value))))]
    (reduce
      (fn [registers instr]
        (let [{dest :dest value :value} instr
              evaluated-value (evaluate-value registers value)]
          (assoc registers dest evaluated-value)))
      (into {} (for [[i idx] (map vector input (range))]
                 [{:reg "in" :idx idx} i]))
      program)))

(defn compare-registers [a b]
  (compare [(:reg a) (:idx a)] [(:reg b) (:idx b)]))


(defn solve [digit-range]
  (let [optimized (doall (optimize input))]
    (str/join (solve-digits optimized 14 digit-range))))

(defn part1 []
  (solve (range 9 0 -1)))

(defn part2 []
  (solve (range 1 10)))

(defn -main []
  (try
    (do
      (println (part1))
      (println (part2)))
    (catch Throwable e (.printStackTrace e))))

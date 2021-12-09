(ns adventofcode2021.advent-util
  "Utility functions for Advent of Code"
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

(defn get-input-file-name [ns]
  "Gets the standard AoC input file name for the provided namespace. The
  namespace is expected to be in the form \"foo.bar.dayXXX\", and the
  corresponding input file will be \"dayXXX.txt\""
  (str (last (str/split (str (ns-name ns)) #"\.")) ".txt"))

(defn read-input
  "Reads the input of the AoC input file. If no filename is provided,
  it will be inferred from *ns*. Returns a string."
  ([]
   (read-input (get-input-file-name *ns*)))
  ([filename]
   (-> filename
       (io/resource)
       (slurp))))

(defn read-input-lines
  "Reads the input of the AoC input file. If no filename is provided,
  it will be inferred from *ns*. Returns a sequence of lines."
  ([]
   (read-input-lines (get-input-file-name *ns*)))
  ([filename]
   (-> filename
       (io/resource)
       (slurp)
       (str/split-lines))))


(defn parse-input-lines
  "Parses the input of the AoC input file. If no filename is provided,
  it will be inferred from *ns*. Each line is parsed by line-parser."
  ([line-parser]
   (parse-input-lines (get-input-file-name *ns*) line-parser))
  ([filename line-parser]
   (for [line (read-input-lines filename)]
     (line-parser line))))

(defn parse-int
  "Parses an int from a string via Integer/parseInt. As a normal Clojure function,
  this can be more easily passed around than Integer/parseInt."
  ([s]
   (Integer/parseInt s))
  ([s radix]
   (Integer/parseInt s radix)))

(defn parse-long
  "Parses a long from a string via Long/parseLong. As a normal Clojure function,
  this can be more easily passed around than Long/parseLong."
  ([s]
   (Long/parseLong s))
  ([s radix]
   (Long/parseLong s radix)))

(defn abs-value
  "Computes the absolute value of a number"
  ([n]
   (if (< n 0)
     (- n)
     n)))

(defn sign
  "Returns -1, 0, or 1 if the value is negative, zero, or positive"
  ([n]
   (cond
     (< n 0) -1
     (> n 0) 1
     :else 0)))

(defn trans-closure
  "Computes the transitive closure of the given function. The result includes
  all the elements in roots, as well as all the elements produced by calling
  (fn e) for every element in the transitive closure (starting with the roots).
  fn is expected to return a seqable type."
  [fn roots]
  (letfn [(helper [seen backlog]
            (lazy-seq
              (when-let [[hd & tl] (seq backlog)]
                (if (seen hd)
                  (helper seen tl)
                  (cons hd (helper (conj seen hd) (concat (fn hd) tl)))))))]
    (helper #{} roots)))

(defn greatest-common-divisor
  "Computes the greatest-common-divisor of the arguments."
  ([a b]
   (cond
     (= a 0) b
     (= b 0) a
     :else (let [remainder (rem a b)]
             (recur b remainder))))
  ([a b & more]
   (reduce
     greatest-common-divisor
     a
     (cons b more))))

(defn iterate-until-stable
  "Like iterate, but stops as soon as f produces output equal to its input."
  [f x]
  (lazy-seq
    (let [n (f x)]
      (if (= n x)
        [x]
        (cons x (iterate-until-stable f n))))))

(defn least-common-multiple
  "Computes the least common multiple of the arguments."
  ([x y] (/ (abs-value (* x y)) (greatest-common-divisor x y)))
  ([x y & more] (reduce least-common-multiple x (cons y more))))

(defn find-dims
  "Returns the [h w] dimensions of the input data, which is assumed to be in row-major format"
  [data-row-major]
  [(count data-row-major) (count (first data-row-major))])

(defn iterate-dims
  "Iterates the [h w] dimensions of the input data, changing x faster than y"
  [dims]
  (let [[h w] dims]
    (for [y (range h)
          x (range w)]
      [y x])))

(defn adjacent-locs
  "Finds the locations adjacent to [y x] within the bounds [0 0] and [h w]"
  [[y x] [h w]]
  (let [candidates [[y (dec x)]
                    [y (inc x)]
                    [(dec y) x]
                    [(inc y) x]]]
    (filter
      (fn [[y x]]
        (and
          (>= x 0)
          (< x w)
          (>= y 0)
          (< y h)))
      candidates)))
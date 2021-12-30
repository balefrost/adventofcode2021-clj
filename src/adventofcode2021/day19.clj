(ns adventofcode2021.day19
  (:use adventofcode2021.advent-util)
  (:use clojure.pprint)
  (:use [clojure.data.priority-map :only [priority-map]])
  (:require [clojure.string :as str]))

(defn parse-scanner [lines]
  (if (seq lines)
    (let [lines (drop-while str/blank? lines)
          [header & lines] lines
          [_ scannerno] (re-matches #"--- scanner (\d+) ---" header)
          scannerno (parse-int scannerno)
          [point-lines lines] (split-with #(re-matches #"(-?\d+),(-?\d+),(-?\d+)" %) lines)
          points (for [line point-lines]
                   (let [[_ & xyz] (re-matches #"(-?\d+),(-?\d+),(-?\d+)" line)]
                     (mapv parse-int xyz)))]
      [{:scanner scannerno
        :points  points}
       lines])))

(defn dot-product [a b]
  (reduce + (map * a b)))

(defn mat-inv [mat]
  (apply mapv vector mat))

(defn mat-multiply
  ([a] a)
  ([a b]
   (let [cols-of-b (mat-inv b)]
     (assert (= (count (first a)) (count b)) (str "incompatible dims: " (count (first a)) " " (count b)))
     (mapv
       (fn [row-of-a]
         (mapv
           (fn [col-of-b]
             (dot-product row-of-a col-of-b))
           cols-of-b))
       a)))
  ([a b & rest]
   (reduce
     mat-multiply
     (mat-multiply a b)
     rest)))

(def rot-around-x
  [[1 0 0]
   [0 0 -1]
   [0 1 0]])

(def rot-around-y
  [[0 0 -1]
   [0 1 0]
   [1 0 0]])

(def rot-around-z
  [[0 -1 0]
   [1 0 0]
   [0 0 1]])

(def mat-identity
  [[1 0 0]
   [0 1 0]
   [0 0 1]])

(def four-rotations-around-z
  [mat-identity
   rot-around-z
   (mat-multiply rot-around-z rot-around-z)
   (mat-multiply rot-around-z rot-around-z rot-around-z)])

(def six-orientations-for-z
  [mat-identity
   rot-around-x
   (mat-multiply rot-around-x rot-around-x)
   (mat-multiply rot-around-x rot-around-x rot-around-x)
   rot-around-y
   (mat-multiply rot-around-y rot-around-y rot-around-y)])

(defn mult-mat-by-point [mat point]
  (mapv
    (fn [row]
      (dot-product row point))
    mat))

(def all-orientations
  (for [a six-orientations-for-z
        b four-rotations-around-z]
    (mat-multiply b a)))

(defn parse-input [input-text]
  (letfn [(helper [lines]
            (lazy-seq
              (if-let [[scanner lines] (parse-scanner lines)]
                (cons scanner (helper lines)))))]
    (helper (str/split-lines input-text))))


(def input (parse-input (read-input)))
;(def input (parse-input "--- scanner 0 ---\n404,-588,-901\n528,-643,409\n-838,591,734\n390,-675,-793\n-537,-823,-458\n-485,-357,347\n-345,-311,381\n-661,-816,-575\n-876,649,763\n-618,-824,-621\n553,345,-567\n474,580,667\n-447,-329,318\n-584,868,-557\n544,-627,-890\n564,392,-477\n455,729,728\n-892,524,684\n-689,845,-530\n423,-701,434\n7,-33,-71\n630,319,-379\n443,580,662\n-789,900,-551\n459,-707,401\n\n--- scanner 1 ---\n686,422,578\n605,423,415\n515,917,-361\n-336,658,858\n95,138,22\n-476,619,847\n-340,-569,-846\n567,-361,727\n-460,603,-452\n669,-402,600\n729,430,532\n-500,-761,534\n-322,571,750\n-466,-666,-811\n-429,-592,574\n-355,545,-477\n703,-491,-529\n-328,-685,520\n413,935,-424\n-391,539,-444\n586,-435,557\n-364,-763,-893\n807,-499,-711\n755,-354,-619\n553,889,-390\n\n--- scanner 2 ---\n649,640,665\n682,-795,504\n-784,533,-524\n-644,584,-595\n-588,-843,648\n-30,6,44\n-674,560,763\n500,723,-460\n609,671,-379\n-555,-800,653\n-675,-892,-343\n697,-426,-610\n578,704,681\n493,664,-388\n-671,-858,530\n-667,343,800\n571,-461,-707\n-138,-166,112\n-889,563,-600\n646,-828,498\n640,759,510\n-630,509,768\n-681,-892,-333\n673,-379,-804\n-742,-814,-386\n577,-820,562\n\n--- scanner 3 ---\n-589,542,597\n605,-692,669\n-500,565,-823\n-660,373,557\n-458,-679,-417\n-488,449,543\n-626,468,-788\n338,-750,-386\n528,-832,-391\n562,-778,733\n-938,-730,414\n543,643,-506\n-524,371,-870\n407,773,750\n-104,29,83\n378,-903,-323\n-778,-728,485\n426,699,580\n-438,-605,-362\n-469,-447,-387\n509,732,623\n647,635,-688\n-868,-804,481\n614,-800,639\n595,780,-596\n\n--- scanner 4 ---\n727,592,562\n-293,-554,779\n441,611,-461\n-714,465,-776\n-743,427,-804\n-660,-479,-426\n832,-632,460\n927,-485,-438\n408,393,-506\n466,436,-512\n110,16,151\n-258,-428,682\n-393,719,612\n-211,-452,876\n808,-476,-593\n-575,615,604\n-485,667,467\n-680,325,-822\n-627,-443,-432\n872,-547,-609\n833,512,582\n807,604,487\n839,-516,451\n891,-625,532\n-652,-548,-490\n30,-46,-14"))

(defn find-all-orientations [points]
  (for [orientation all-orientations]
    {:mat    orientation
     :points (for [point points]
               (mult-mat-by-point orientation point))}))

(defn in-range [range pt]
  (let [[x y z] pt
        [[x1 x2] [y1 y2] [z1 z2]] range]
    (and
      (<= x1 x x2)
      (<= y1 y y2)
      (<= z1 z z2))))

(defn find-overlap [a b]
  (let [possible-offsets (distinct
                           (for [anchor a
                                 target b]
                             (mapv - anchor target)))]
    (first
      (for [[b2a-x b2a-y b2a-z :as b-to-a] possible-offsets
            :let [overlap-in-a [[(max -1000 (+ b2a-x -1000))
                                 (min 1000 (+ b2a-x 1000))]
                                [(max -1000 (+ b2a-y -1000))
                                 (min 1000 (+ b2a-y 1000))]
                                [(max -1000 (+ b2a-z -1000))
                                 (min 1000 (+ b2a-z 1000))]]]
            :let [relevant-a (into #{} (filter #(in-range overlap-in-a %)) a)]
            :when (>= (count relevant-a) 12)
            :let [b-in-a (map #(mapv + b-to-a %) b)]
            :let [relevant-b-in-a (into #{} (filter #(in-range overlap-in-a %)) b-in-a)]
            :when (= relevant-a relevant-b-in-a)]
        {:scanner-pos        b-to-a
         :aligned-points     b-in-a
         :overlapping-points relevant-b-in-a}))))

(defn add-vector [a b]
  (mapv + a b))

(def solution
  (delay
    (let [ze-orientations (into {} (for [a input]
                                     [(:scanner a) (find-all-orientations (:points a))]))
          alignment-entry (first input)
          other-entries (rest input)
          [final-points final-scanners] (loop [to-orient (set (map :scanner other-entries))
                                               orientation-bases [(:scanner alignment-entry)]
                                               points (set (:points alignment-entry))
                                               oriented-solved {(:scanner alignment-entry) {:scanner-pos      [0 0 0]
                                                                                            :unaligned-points (:points alignment-entry)
                                                                                            :aligned-points   (:points alignment-entry)
                                                                                            :mat              mat-identity}}]
                                          (println (count oriented-solved) " / " (count input))
                                          (if (and (not-empty orientation-bases) (not-empty to-orient))
                                            (let [[orientation-base-no & orientation-bases] orientation-bases
                                                  existing-solution (get oriented-solved orientation-base-no)
                                                  oriented-base-points (get existing-solution :unaligned-points)
                                                  solved (for [b-no to-orient
                                                               orientation (get ze-orientations b-no)
                                                               :let [oriented-points (:points orientation)]
                                                               :let [aligned (find-overlap oriented-base-points oriented-points)]
                                                               :when aligned]
                                                           {:base-scanner     orientation-base-no
                                                            :other-scanner    b-no
                                                            :scanner-pos      (add-vector (:scanner-pos existing-solution) (:scanner-pos aligned))
                                                            :unaligned-points oriented-points
                                                            :aligned-overlappingpoints (for [p (:overlapping-points aligned)]
                                                                                         (add-vector (:scanner-pos existing-solution) p))
                                                            :aligned-points   (for [p (:aligned-points aligned)]
                                                                                (add-vector (:scanner-pos existing-solution) p))
                                                            :mat              (:mat orientation)})
                                                  to-orient (reduce
                                                              disj
                                                              to-orient
                                                              (map :other-scanner solved))
                                                  orientation-bases (concat orientation-bases (map :other-scanner solved))
                                                  points (into points (for [s solved
                                                                            pt (:aligned-points s)]
                                                                        pt))
                                                  oriented-solved (into oriented-solved
                                                                        (map
                                                                          (fn [s]
                                                                            [(:other-scanner s)
                                                                             s]))
                                                                        solved)]
                                              (recur to-orient orientation-bases points oriented-solved))
                                            [points (map :scanner-pos (vals oriented-solved))]))]
      {:points final-points
       :scanners final-scanners})))

(defn part1 []
  (count (:points @solution)))

(defn manhattan-distance [a b]
  (reduce + 0 (map #(abs-value (- %1 %2)) a b)))

(defn part2 []
  (reduce
    max
    0
    (for [a (:scanners @solution)
          b (:scanners @solution)]
      (manhattan-distance a b))))

(defn -main []
  (try
    (do
      (println (part1))
      (println (part2)))
    (catch Exception e (.printStackTrace e))))


(ns aoc2022.day8)

(defn char-to-digit
  [char]
  (Character/digit char 10))
(defn a
  []
  (with-open [rdr (clojure.java.io/reader (aoc2022.core/input-file "8"))]
    (->> (line-seq rdr)
         (map char-array)
         (map #(map char-to-digit %))
         (vec)
         )))
(defn visible-trees
  [line]
  (loop [l (rest line)
         highest (first line)
         acc [true]]
    (if (seq l)
      (recur (rest l)
             (max highest (first l))
             (conj acc (if (> (first l) highest)
                         true
                         false)))
      acc)))

(defn visible-trees-both-ways
  [line]
  (->> (reverse line)
       (visible-trees)
       (reverse)
       (map #(or %1 %2) (visible-trees line))))

(defn transpose-matrix
  [m]
  (apply mapv vector m))

(defn my-or
  [x y]
  (or x y))

(defn solve1
  []
  (let [tree1 (map visible-trees-both-ways (a))
        tree2 (->> (a)
                   (transpose-matrix)
                   (map visible-trees-both-ways)
                   (transpose-matrix))]
    (->> (map #(map my-or %1 %2) tree1 tree2)
         (map (partial filter true?))
         (map count)
         (reduce +))))

(defn scenic
  [line]
  (let [height (first line)]
    (loop [[x & xs :as l] (rest line)
           acc 0]
      (if (seq l)
        (cond
          (>= x height) (inc acc)
          (< x height) (recur xs
                              (inc acc)))
        acc))))

(defn scenic-from
  [line n]
  (->> (drop n line)
       (scenic)))

(defn scenic-from-both
  [line n]
  (* (scenic-from line n)
     (scenic-from (reverse line)
                  (-> (count line)
                      (dec)
                      (- n)))))

(defn scenic-line
  [line]
  (map scenic-from-both (repeat (count line) line) (range 0 (count line))))

(defn all-scenic-scores
  []
  (map (partial map *)
       (->> (a)
            (map scenic-line))
       (->> (a)
            (transpose-matrix)
            (map scenic-line)
            (transpose-matrix))))

(defn solve2
  []
  (->> (all-scenic-scores)
       (map (partial reduce max))
       (reduce max)))

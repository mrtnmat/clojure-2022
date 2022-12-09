(ns aoc2022.day7
  (:require [aoc2022.core]))

(defn dir-size
  [dir]
  (->> (tree-seq map? vals dir)
       (remove map?)
       (reduce +)
       ))

(defn find-dirs
  [state]
  (let [a (tree-seq map? #(interleave (keys %) (vals %)) (state :dir-tree))]
    (->> (map vector
              (take-nth 2 (drop 1 a))
              (take-nth 2 (drop 2 a)))
         (filter #(map? (second %)))
         )))

(defn cd
  ([dir-name]
   (cd dir-name {:current-dir (list) :dir-tree {:/ {}}}))
  ([state dir-name]
   (if (= dir-name "..")
     (update state :current-dir pop)
     (->> dir-name
          (keyword)
          (update-in state [:current-dir] conj)))))

(defn mkdir
  [state dir-name]
  (update-in state
             (cons :dir-tree (reverse (state :current-dir)))
             #(assoc % (keyword dir-name) {})))

(defn touch
  [state file-name size]
  (update-in state
             (cons :dir-tree (reverse (state :current-dir)))
             #(assoc % (keyword file-name) size)))

(defn parse-line
  [state line]
  (case (subs line 0 4)
    "$ cd" (cd state (re-find #"(?<=cd ).+" line))
    "$ ls" state
    "dir " (mkdir state (subs line 4))
    (let [l (clojure.string/split line #"\s")
          name (second l)
          size (parse-long (first l))]
      (touch state name size))))

(defn all-dirs-size
  []
  (with-open [rdr (clojure.java.io/reader (aoc2022.core/input-file "7"))]
    (->> (line-seq rdr)
         (reduce parse-line {:current-dir (list)
                             :dir-tree    {:/ {}}})
         (find-dirs)
         (map (fn [v] (update v 1 dir-size))))))

(defn solve1
  []
  (->> (all-dirs-size)
       (map second)
       (filter (partial > 100000))
       (reduce +)))

(def total-space 70000000)
(def space-required 30000000)

(defn solve2
  []
  (let [sorted-dirs (->> (all-dirs-size)
                         (sort #(< (second %1) (second %2))))]
    (->> sorted-dirs
         (drop-while #(> (- (second (last sorted-dirs)) (- total-space space-required)) (second %)))
         (first))))

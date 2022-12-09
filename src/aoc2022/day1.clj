(ns aoc2022.day1
  (:require [clojure.string :as s]))

(defn toInt [s]
  (Integer/parseInt s))

(defn calories []
  (let [t (slurp "input/day1")]
    (->> (re-seq #"(?:\d+\n{1})+" t)
         (map #(->> (s/split-lines %)
                    (map toInt)
                    (reduce +)
                    )))))

(defn part1 []
  (reduce max (calories)))

(defn part2 []
  (->> (calories)
       (sort >)
       (take 3)
       (reduce +)))

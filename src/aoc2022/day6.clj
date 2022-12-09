(ns aoc2022.day6
  (:require [aoc2022.core]))


(defn all-unique [s]
  (->> s
       (frequencies)
       (vals)
       (some #(not (= % 1)))
       (not)))

(defn solver [input length]
  (loop [ans length
         inp (seq input)]
    (when inp
      (if (all-unique (take length inp))
        ans
        (recur (inc ans)
               (seq (drop 1 inp)))))))

(defn solver2 [input length]
  (->> input
       (iterate (partial drop 1))
       (map (partial take length))
       (take-while #(= (count %) length))
       (map all-unique)
       (take-while false?)
       (count)
       (+ length)))

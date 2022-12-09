(ns aoc2022.day2
  (:require [aoc2022.core :refer [!=]]
            [clojure.string]
            [clojure.java.io :as io]))

(def k {"A" :rock
        "B" :paper
        "C" :scissors
        "X" :rock
        "Y" :paper
        "Z" :scissors})

(defn parse-turn [t]
  (let [a (->> (clojure.string/split t #"\s")
               (map k))]
    {:me (second a)
     :him (first a)}))

(def win-chart {:rock {:rock :draw
                       :paper :loss
                       :scissors :win}
                :paper {:rock :win
                        :paper :draw
                        :scissors :loss}
                :scissors {:rock :loss
                           :paper :win
                           :scissors :draw}})

(defn result [{me :me him :him}]
  ((win-chart me) him))

(defn score [turn]
  (let [match (parse-turn turn)
        symbol {:rock 1 :paper 2 :scissors 3}
        outcome {:loss 0 :draw 3 :win 6}]
    (->> (match :me)
         (symbol)
         (+ (outcome (result match))))))

(defn part1 []
  (with-open [rdr (io/reader "input/day2")]
    (->> (line-seq rdr)
         (map score)
         (reduce +))))

(defn parse-turn2 [t]
  (let [v {"A" :rock "B" :paper "C" :scissors
           "X" :loss "Y" :draw "Z" :win}
        a (->> (clojure.string/split t #"\s")
               (map v))]
    {:result (second a)
     :him (first a)}))

(defn what-to-play [{result :result
                     him :him :as match}]
  (let [steps {:draw 0 :win 1 :loss 2}]
    (->> (map repeat [:rock :paper :scissors])
         (apply interleave)
         (drop-while #(!= % him))
         (drop (steps result))
         (first)
         (assoc match :me))))

(defn score2 [turn]
  (let [match (->> turn
                   (parse-turn2)
                   (what-to-play))
        points {:loss 0 :draw 3 :win 6
                :rock 1 :paper 2 :scissors 3}]
    (+ (points (match :me))
       (points (match :result)))))

(defn part2 []
  (with-open [rdr (io/reader "input/day2")]
    (->> (line-seq rdr)
         (map score2)
         (reduce +))))
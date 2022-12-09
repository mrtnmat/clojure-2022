(ns aoc2022.core
  (:require [clojure.java.io :as io]))

(defn != [a b]
  (not (= a b)))

(defn inspect [x]
  (println x)
  x)

(defn input-file
  [day-number]
  (io/file "e:\\" "Projects" "aoc" "2022" (apply str ["day" day-number ".txt"])))

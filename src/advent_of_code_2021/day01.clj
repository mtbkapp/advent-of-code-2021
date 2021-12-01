;# [Day 01](https://adventofcode.com/2021/day/1) 

(ns advent-of-code-2021.day01
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]))

; Read the my puzzle input 
(def real-input (slurp (io/resource "day01.txt")))

; Same format for the test input
(def test-input
  "199
200
208
210
200
207
240
269
260
263")


(defn read-input
  [s]
  (map #(Long/parseLong %) (string/split-lines s)))


; Pairing each element with it's previous and ignoring the first element is the 
; same as pairing each element with it's next element. This is can be done with
; `clojure.core/partition`.

(defn solve-part1
  [input]
  (->> input
       (partition 2 1)
       (filter (fn [[prev curr]]
                 (> curr prev)))
       (count)))

(solve-part1 (read-input test-input))
(solve-part1 (read-input real-input))


; Partition can also be used to make the windows as described in part2. Then the
; same algorithm from part1.
(defn solve-part2
  [input]
  (->> (partition 3 1 input)
       (map #(apply + %))
       (solve-part1)))

(solve-part2 (read-input test-input))
(solve-part2 (read-input real-input))


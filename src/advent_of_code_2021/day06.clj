;# [Day 06](https://adventofcode.com/2021/day/6) 

(ns advent-of-code-2021.day06
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]))


; read the input into strings
(def real-input (slurp (io/resource "day06.txt")))

(def test-input "3,4,3,1,2")


; A space efficient way to store the number of days until a fish sprouts 
; another fish is in a vector where the first element is the number of fish
; with that will sprout a new fish in 0 days, the second is in 1 day, etc. 
(defn read-input
  [input]
  (transduce (map #(Long/valueOf (string/trim %)))
             (completing
               (fn [fish age]
                 (update fish age inc)))
             [0 0 0 0 0 0 0 0 0]
             (string/split input #",")))


(read-input test-input)
(read-input real-input)


; Clojure doesn't have a rotate function (that I know of) so here is one that
; rotates a vector left. This simulates number of days for each group of fish
; decreasing by one
(defn rotate-vec
  [v]
  (conj (subvec v 1) (first v)))


; One step is just rotating and incrementing the number of fish with 6 days left
; by the number of newly sprouted fish which is in index 8. 
(defn step
  [fish]
  (let [r (rotate-vec fish)]
    (update r 6 + (nth r 8))))


; Count the total number of fish after some number of days of simulation.
(defn count-at-day
  [day input]
  (->> (iterate step input)
       (drop day)
       (first)
       (apply +)))


(count-at-day 80 (read-input test-input))
(count-at-day 80 (read-input real-input))
(count-at-day 256 (read-input test-input))
(count-at-day 256 (read-input real-input))



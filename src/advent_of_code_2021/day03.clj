;# [Day 03](https://adventofcode.com/2021/day/3) 

(ns advent-of-code-2021.day03
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]))


; read the input into strings
(def real-input (slurp (io/resource "day03.txt")))

(def test-input "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010")

; parse each line into a vector of characters
(defn read-input
  [input]
  (map vec (string/split-lines input)))


; for a specific digit return a map of each unique digit value to the number 
; of times that value appears in the input.
(defn single-digit-freq
  [input i]
  (frequencies (map #(nth % i) input)))


; same as `single-digit-freq` but for all digits.
(defn digit-frequencies
  [input]
  (map #(single-digit-freq input %) 
       (range (count (first input)))))


; given a frequency map returns the most common value
(defn gamma 
  [{zero-count \0 one-count \1}]
  (if (> zero-count one-count)
    \0 
    \1))


; given a frequency map returns the least common value
(defn epsilon
  [{zero-count \0 one-count \1}]
  (if (> zero-count one-count)
    \1 
    \0))


; given a vector of binary digits return it's decimal equivalent 
(defn vec->long
  [v]
  (Long/valueOf (apply str v) 2))


; given which function to use (gamma or epsilon) and the frequency maps
; for each digit calculate the rate
(defn calc-rate
  [f freqs]
  (vec->long (map f freqs)))


; finally solve part1
(defn solve-part1
  [input]
  (let [freqs (digit-frequencies input)]
    (* (calc-rate gamma freqs)
       (calc-rate epsilon freqs))))


(solve-part1 (read-input test-input))
(solve-part1 (read-input real-input))


(defn digit-filter
  [i v]
  #(= v (nth % i)))


; do the filtering process as described in the problem statement.
(defn calc-rating
  [f input]
  (loop [i 0 input input]
    (if (= 1 (count input))
      (vec->long (first input))
      (recur (inc i)
             (filter (digit-filter i (f (single-digit-freq input i)))
                     input)))))


(defn solve-part2
  [input]
  (* (calc-rating gamma input)
     (calc-rating epsilon input)))


(solve-part2 (read-input test-input))
(solve-part2 (read-input real-input))


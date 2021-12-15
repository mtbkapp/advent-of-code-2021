;# [Day 14](https://adventofcode.com/2021/day/14)

(ns advent-of-code-2021.day14
  (:require [clojure.java.io :as io]
            [clojure.set :as sets]
            [clojure.string :as string]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]))


(def real-input (slurp (io/resource "day14.txt")))


(def test-input
  "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C")

; Turns out that only count of each pair needs to be maintained. And the last
; element which never changes.
(defn read-input
  [input]
  (let [[temp sbs] (string/split input #"\n\n")]
    (let  [ps (partition 2 1 temp)]
      {:pairs (frequencies ps) 
       :last-el (last temp)
       :sbs (into {}
                  (comp (map #(string/split % #" \-\> "))
                        (map (fn [[[x y] [z]]]
                               [[x y] [[x z] [z y]]])))
                  (string/split-lines sbs))})))


(read-input test-input)

; Given the current pair counts create new ones using the substitution rules.
;
; Example: Given the rule `XY -> Z` the `XY` pairs are replaced with the same
;          number of `XZ` and `ZY` pairs.
(defn next-pairs
  [pairs sbs]
  (reduce (fn [np [p c]]
            (let [[np1 np2] (get sbs p)]
              (-> np
                  (update np1 (fnil + 0) c)
                  (update np2 (fnil + 0) c))))
          {}
          pairs))


; Given the current state create the next state.
(defn step
  [{:keys [pairs sbs] :as input}]
  (update input :pairs next-pairs sbs))


; Run the simulation `n` steps.
(defn run
  [input n]
  (->> (iterate step input)
       (drop n)
       first))


; Return a map of elements to the number of times they appear in the pairs.
; Since the second element in a pair is the same as the first element in another
; pair only consider the first element in each pair. To ensure the proper count
; for the last element start with it's count as 1.
(defn count-els
  [{:keys [pairs last-el] :as input}]
  (reduce (fn [freqs [[a _] c]]
            (update freqs a (fnil + 0) c))
          {last-el 1}
          pairs))


(defn most-minus-least
  [input]
  (let [counts (->> input count-els (sort-by val))]
    (- (val (last counts))
       (val (first counts)))))


(defn solve-part1
  [input]
  (most-minus-least (run input 10)))


(solve-part1 (read-input test-input))
(solve-part1 (read-input real-input))


(defn solve-part2
  [input]
  (most-minus-least (run input 40)))


(solve-part2 (read-input test-input))
(solve-part2 (read-input real-input))

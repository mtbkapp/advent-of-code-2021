;# [Day 02](https://adventofcode.com/2021/day/2) 

(ns advent-of-code-2021.day02
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]))


; read the input into strings
(def real-input (slurp (io/resource "day02.txt")))

(def test-input "forward 5
down 5
forward 8
up 3
down 8
forward 2")


; Parses the string input into a sequence of direction, magnitude pairs.
(defn read-input
  [input]
  (map (fn [line]
         (let [[dir mag] (string/split line #"\s")]
           [(keyword dir) (Long/valueOf mag)]))
       (string/split-lines input)))


; Does it work?
(read-input test-input)


; Given a current position and a direction/magnitude pair return the new position
(defn apply-dir
  [pos [dir mag]]
  (update pos
          (if (= dir :forward) :h :depth)
          +
          (if (= dir :up) (- mag) mag)))


; Solve part one by reducing over the parsed input
(defn solve-part1
  [input]
  (let [{:keys [h depth]} (reduce apply-dir
                                  {:h 0 :depth 0}
                                  input)]
    (* h depth)))

(solve-part1 (read-input test-input))
(solve-part1 (read-input real-input))

; Part 2 with updated rules
(defn apply-dir
  [{:keys [aim] :as pos} [dir mag]]
  (if (= :forward dir)
    (-> pos
        (update :h + mag)
        (update :depth + (* aim mag)))
    (update pos :aim + (if (= :up dir) (- mag) mag))))


(defn solve-part2
  [input]
  (let [{:keys [h depth]} (reduce apply-dir
                                  {:aim 0 :h 0 :depth 0}
                                  input)]
    (* h depth)))


(solve-part2 (read-input test-input))
(solve-part2 (read-input real-input))

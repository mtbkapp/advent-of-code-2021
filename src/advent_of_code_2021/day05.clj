;# [Day 05](https://adventofcode.com/2021/day/5) 

(ns advent-of-code-2021.day05
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]))


; read the input into strings
(def real-input (slurp (io/resource "day05.txt")))

(def test-input "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")


(defn read-input
  [input]
  (map (fn [line]
         (let [[matches] (re-seq #"(\d+),(\d+) \-\> (\d+),(\d+)" line)
               [x0 y0 x1 y1] (map #(Long/valueOf %) (rest matches))]
           [[x0 y0] [x1 y1]]))
       (string/split-lines input)))


; Add 2 2d vectors.
(defn vec+
  [[x0 y0] [x1 y1]]
  [(+ x0 x1) (+ y0 y1)])

; One way to draw a line is to get a unit vector between the two points. Start
; at the first point and add the unit vector some number of times until you
; arrive at the second point. That is what is done here except that the unit
; vector isn't a true unit vector because it doesn't always have a length of 1.
; Because of the constraint that the lines can only have slopes of 0, 1, -1, and
; vertical (1/0 slope) the easiest way is to have potential unit vectors of
; where each element can be 0, 1, or -1. It's more of a direction than a true
; unit vector.
(defn unit
  [a b]
  (cond (< a b) 1
        (> a b) -1
        :else 0))


(defn unit-vec 
  [[x0 y0] [x1 y1]]
  ;
  [(unit x0 x1)
   (unit y0 y1)])


(defn draw-line
  ([[start end]]
   (draw-line start end (unit-vec start end) []))
  ([start end diff points]
   (if (= start end)
     (conj points start)
     (recur (vec+ start diff)
            end
            diff
            (conj points start)))))


; To count overlapping just count how many of the same point there are and only
; include those that appear more than once.
(defn count-overlapping
  [points]
  (->> points
       frequencies
       (filter #(<= 2 (val %)))
       count))


; In this implementation the solution to part1 turns out to be a special case
; of part2 because they are the same except part1 requires extra filtering.
; In both cases, the idea is to take the input lines and get the points between
; them and put them all in the same collection and then count the frequency of
; each.
(defn solve-part1
  [input]
  (count-overlapping
    (into []
          (comp (filter (fn [[[x0 y0] [x1 y1]]]
                          (or (= x0 x1)
                              (= y0 y1))))
                (mapcat draw-line))
          input)))


(solve-part1 (read-input test-input))
(solve-part1 (read-input real-input))


(defn solve-part2
  [input]
  (count-overlapping (mapcat draw-line input)))


(solve-part2 (read-input test-input))
(solve-part2 (read-input real-input))


;# [Day 11](https://adventofcode.com/2021/day/11)

(ns advent-of-code-2021.day11
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]))


(def real-input (slurp (io/resource "day11.txt")))


(def test-input "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526")


(def size 10)


(defn vec+
  [[x0 y0] [x1 y1]]
  [(+ x0 x1) (+ y0 y1)])


(defn adjacent
  [p]
  (for [dy [-1 0 1] 
        dx [-1 0 1] 
        :let [[x y :as ap] (vec+ p [dx dy])]
        :when (and (not= p ap)
                   (< -1 x size)
                   (< -1 y size))]
    ap))


(defn read-input
  [input]
  (mapv #(mapv (fn [line] (Long/valueOf (str line))) %)
        (string/split-lines input)))


(def all-points
  (for [y (range size)
        x (range size)]
    [x y]))


(defn inc-energy-at
  [octos [x y]]
  (update-in octos [y x] inc))


(defn set-energy-at
  [octos [x y] e]
  (assoc-in octos [y x] e))


(defn inc-energy
  ([octos] (inc-energy octos all-points))
  ([octos points]
   (reduce inc-energy-at octos points)))


(defn get-energy
  [octos [x y]]
  (get-in octos [y x]))


(defn find-flashers
  ([octos]
   (find-flashers octos all-points))
  ([octos points]
   (filter #(< 9 (get-energy octos %)) points)))

; Almost all the logic for this solution are in this function. It's sort of
; like a breadth first search. The queue starts with all the octopuses that
; will flash. Then while the queue has octopuses in it an octopus is pulled
; from the queue and:
; * if the energy level of the octopus is 0 then it was already processed so
;   skip it.
; * energy level set to 0
; * energy level of all adjacent octopuses is incremented unless it is 0, which
;   means that it flashed during this step.
; * the adjacent octopuses that have their energy level higher than 9 are 
;   put into the queue
(defn prop-flashes
  [octos]
  (loop [q (into clojure.lang.PersistentQueue/EMPTY (find-flashers octos))
         octos octos]
    (if (empty? q)
      octos
      (let [[x y :as p] (peek q)
            nq (pop q)]
        (if (= 0 (get-energy octos p))
          (recur nq octos)
          (let [adjs (adjacent p)
                octos2 (reduce (fn [os [x y :as p]]
                                 (if (= 0 (get-energy os p))
                                   os
                                   (inc-energy-at os p)))
                               (set-energy-at octos p 0)
                               adjs)
                adj-flashers (find-flashers octos2 adjs)]
            (recur (into nq adj-flashers) octos2)))))))


(def step (comp prop-flashes inc-energy))

; For a given state of octopuses count how many flashed during the last step.
(defn count-flashes
  [state]
  (count (sequence (comp (mapcat identity)
                         (filter #(= 0 %)))
                   state)))


; Use `clojure.core/iterate` to build each state from the last state. Take the 
; first 101 states. Then count the flashes in each and sum them.
(defn solve-part1
  [input]
  (transduce (comp (take 101)
                   (map count-flashes))
             +
             (iterate step input)))

(solve-part1 (read-input test-input))
(solve-part1 (read-input real-input))


; Just like part1 use `clojure.core/iterate` to build the state at each step
; from the last state. In this case use the fact that `iterate` is lazy and
; keep dropping states until all the octopuses flash at the same time. Use
; `map-indexed` to label each state with which step produced it.
(defn solve-part2
  [input]
  (->> (iterate step input)
       (map-indexed vector)
       (drop-while #(< (count-flashes (second %)) 100))
       (ffirst)))

(solve-part2 (read-input test-input))
(solve-part2 (read-input real-input))


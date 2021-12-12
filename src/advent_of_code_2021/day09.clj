;# [Day 09](https://adventofcode.com/2021/day/9)

(ns advent-of-code-2021.day09
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]))


(def real-input (slurp (io/resource "day09.txt")))


(def test-input
  "2199943210
3987894921
9856789892
8767896789
9899965678")


; Read the input map into a vector of vectors--like a 2d array.
(defn read-input
  [input]
  (let [m (mapv (fn [line]
                   (mapv #(Long/valueOf (str %)) line))
                 (string/split-lines input))]
    {:m m
     :h (count m)
     :w (count (first m))}))


(read-input test-input)
(read-input real-input)

; Get adjacent points that are in bounds by doing vector addition with the point
; and the vectors `#{[1 0] [-1 0] [0 1] [0 -1]}` and checking that they are in
; bounds
(defn vec+
  [[x0 y0] [x1 y1]]
  [(+ x0 x1) (+ y0 y1)])


(defn adjacent
  [{:keys [h w] :as input} p]
  (into []
        (comp (map #(vec+ p %))
              (filter (fn [[x y]]
                        (and (< -1 x w)
                             (< -1 y h)))))
        #{[1 0] [-1 0] [0 1] [0 -1]}))


(defn height-at
  [m [x y]]
  (get-in m [y x]))


; Predicate function to determine if some point on the map is a low point.
(defn low?
  [{:keys [m h w] :as input} p]
  (let [height-at-p (height-at m p)]
    (->> (adjacent input p)
         (map (partial height-at m))
         (every? #(> % height-at-p)))))


; Get all low points on the map.
(defn low-points
  [{:keys [m h w] :as input}]
  (filter #(low? input %)
          (for [y (range h)
                x (range w)]
            [x y])))


(low-points (read-input test-input))


(defn solve-part1
  [{:keys [m h w] :as input}]
  (transduce (map #(inc (height-at m %)))
             +
             (low-points input)))


(solve-part1 (read-input test-input))
(solve-part1 (read-input real-input))


; Get the points in the basin starting at some low point by doing a breadth
; first search.
(defn basin-at
  [{:keys [m] :as input} low-point]
  (loop [basin #{}
         q (conj clojure.lang.PersistentQueue/EMPTY low-point)]
    (let [p (peek q)
          nq (pop q)]
      (cond (nil? p) basin
            (or (= 9 (height-at m p)) (contains? basin p)) (recur basin nq)
            :else (recur (conj basin p)
                         (into nq (adjacent input p)))))))


(basin-at (read-input test-input) [9 0])


(defn solve-part2
  [input]
  (->> (low-points input)
       (map #(count (basin-at input %)))
       (sort-by -)
       (take 3)
       (reduce *)))


(solve-part2 (read-input test-input))
(solve-part2 (read-input real-input))

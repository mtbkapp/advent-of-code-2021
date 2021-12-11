;# [Day 09](https://adventofcode.com/2021/day/9) 

(ns advent-of-code-2021.day09
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]))


; read the input into strings
(def real-input (slurp (io/resource "day09.txt")))


(def test-input
  "2199943210
3987894921
9856789892
8767896789
9899965678")


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


(defn vec+
  [[x0 y0] [x1 y1]]
  [(+ x0 x1) (+ y0 y1)])


(defn height-at
  [m [x y]]
  (get-in m [y x]))


(defn low? 
  [{:keys [m h w]} p]
  (let [height-at-p (height-at m p)]
    (->> #{[-1 0] [1 0] [0 -1] [0 1]}
         (map #(vec+ p %))
         (filter (fn [[x y]]
                   (and (< -1 x w)
                        (< -1 y h))))
         (map (partial height-at m))
         (every? #(> % height-at-p)))))


(defn solve-part1
  [{:keys [m h w] :as input}]
  (transduce (comp (filter #(low? input %))
                   (map #(inc (height-at m %))))
             + 
             (for [y (range h)
                   x (range w)]
               [x y])))


(solve-part1 (read-input test-input))
(solve-part1 (read-input real-input))

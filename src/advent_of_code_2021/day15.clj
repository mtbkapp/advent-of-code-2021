;# [Day 15](https://adventofcode.com/2021/day/15)

(ns advent-of-code-2021.day15
  (:require [clojure.java.io :as io]
            [clojure.set :as sets]
            [clojure.string :as string]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]
            [ubergraph.core :as uber]
            [ubergraph.alg :as ualg]))


(def real-input (slurp (io/resource "day15.txt")))


(def test-input
  "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581")


(defn read-input
  [input]
  (let [m (mapv #(mapv (fn [c](Long/valueOf (str c))) %)
                (string/split-lines input))]
    {:m m
     :size (count m)}))


; ## Part 1
; For this one I decided to try to hit the easy button by using the 
; [ubergraph library](https://github.com/Engelberg/ubergraph). Then I modeled
; the risk map into a graph where each location in the map is a node and there
; are edges between each location and it's adjacent locations (up, down, left, 
; right). The weights of the edges into a location it's risk level from the map.
; Then just use the algorithm in ubergraph to find the shortest path and the
; cost of that path.

; Look vector addition again...
(defn vec+
  [[x0 y0] [x1 y1]]
  [(+ x0 x1) (+ y0 y1)])


(defn points-in-square
  [size]
  (for [y (range size)
        x (range size)]
    [x y]))


(defn adjacent
  [[x y :as n] size]
  (for [dv [[-1 0] [1 0] [0 -1] [0 1]]
        :let [[nx ny :as nn] (vec+ n dv)]
        :when (and (< -1 nx size)
                   (< -1 ny size))]
    nn))


(defn build-edges
  [{:keys [m size] :as input} nodes]
  (mapcat (fn [[x y :as n]]
            (map (fn [adj]
                   [adj n (get-in m [y x])])
                 (adjacent n size)))
          nodes))


(defn build-graph
  [{:keys [m size] :as input}]
  (let [nodes (points-in-square size)]
    (as-> (uber/digraph) $
      (apply uber/add-nodes $ nodes)
      (apply uber/add-edges $ (build-edges input nodes)))))


(defn solve-part1
  [{:keys [size] :as input}]
  (-> (build-graph input)
      (ualg/shortest-path [0 0] [(dec size) (dec size)] :weight)
      (ualg/cost-of-path)))


(solve-part1 (read-input test-input))
(solve-part1 (read-input real-input))


; ## Part 2
; I think I got lucky after the polymerization and lantern fish ones because
; the same algorithm still works in Part 2. It's slow but it completes in a
; reasonable amount of time. Just need to expand the map as the puzzle explains.


(defn risk+
  [r x]
  (inc (mod (dec (+ r x)) 9)))


(defn build-tile
  [m size [tx ty]]
  (let [dr (+ tx ty)]
    (reduce (fn [nm [x y]]
              (update-in nm [y x] risk+ dr))
            m
            (points-in-square size))))


(defn blank-map 
  [size]
  (vec (repeat size (vec (repeat size nil)))))


(defn combine-tiles
  [tiles tile-size]
  (reduce (fn [m [x y]]
            (let [in-tile-x (mod x tile-size)
                  in-tile-y (mod y tile-size)
                  tile-x (quot x tile-size)
                  tile-y (quot y tile-size)
                  risk-in-tile (get-in tiles 
                                       [[tile-x tile-y] in-tile-y in-tile-x])]
              (assoc-in m [y x] risk-in-tile)))
          (blank-map (* tile-size 5))
          (points-in-square (* tile-size 5))))


(defn expand-map
  [m size]
  (-> (into {}
            (map #(vector % (build-tile m size %)))
            (points-in-square 5))
      (combine-tiles size)))


(defn init-part2
  [{:keys [m size] :as input}]
  {:m (expand-map m size)
   :size (* size 5)})


(defn solve-part2
  [input]
  (solve-part1 (init-part2 input)))


(solve-part2 (read-input test-input))
(solve-part2 (read-input real-input))


;# [Day 07](https://adventofcode.com/2021/day/7) 

(ns advent-of-code-2021.day07
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]))

; Read the puzzle input 
(def real-input (slurp (io/resource "day07.txt")))

(def test-input "16,1,2,0,4,2,7,1,2,14")

(defn read-input
  [input]
  (mapv #(Long/valueOf (string/trim %)) (string/split input #",")))


(read-input test-input)
(read-input real-input)


; Part 1 fuel usage calculation.
(defn fuel-to-1
  [crabs x]
  (transduce (map #(Math/abs (- x %)))
             +
             crabs))


(fuel-to-1 (read-input test-input) 10)


; It also turns out that the shape of the horizontal position to fuel function 
; looks like parabola:
(defn viz-fuel-usage
  [fuel-to]
  (let [crabs (read-input test-input)
        data (map (fn [x]
                    {:fuel (fuel-to crabs x)
                     :x x})
                  (range (reduce min crabs)
                         (inc (reduce max crabs))))]

    (v/vl {:data {:values data}
           :mark "line"
           :encoding {:x {:field :x :type "quantitative"}
                      :y {:field :fuel :type "quantitative"}}})))

(viz-fuel-usage fuel-to-1)


; Brute force isn't too slow but it is slow. Since the shape of the function is
; known I can use something like gradient decent to find the minimum:
; 1.  pick a starting position and calculate the fuel used to get there.
; 2.  for the position immediately left of the starting position calculate the
;     the fuel used to get there.
; 3.  if the fuel used in step 1 is less that in step 2 start moving right
;     otherwise move left.
; 4.  keep moving and calculating fuel until the fuel used starts to go up
; 5.  that is the optimal position.

; My initial hunch was that maybe the mean of all the positions would be the 
; optimal position. It's not. But I'll use it for the starting position.
(defn start-x
  [crabs]
  (long (/ (reduce + crabs)
           (count crabs))))

; The gradient decent like algorithm:
(defn min-fuel
  [crabs fuel-to]
  (let [x (start-x crabs)
        fx (fuel-to crabs x)
        left (fuel-to crabs (dec x))
        dx (if (< left fx) -1 1)]
    (loop [x x
           fuel fx]
      (let [nx (+ x dx)
            n-fuel (fuel-to crabs nx)]
        (if (< n-fuel fuel)
          (recur nx n-fuel)
          [x fuel])))))

; Part 1 Solutions
(min-fuel (read-input test-input) fuel-to-1)
(min-fuel (read-input real-input) fuel-to-1)


(defn fuel-to-2
  [crabs x]
  (transduce (map #(->> (- x %)
                        (Math/abs)
                        (inc)
                        (range)
                        (apply +)))
             +
             crabs))

; Visualize the fuel usage for the test dataset.
(viz-fuel-usage fuel-to-2)

; Part 2 Solutions
(min-fuel (read-input test-input) fuel-to-2)
(min-fuel (read-input real-input) fuel-to-2)

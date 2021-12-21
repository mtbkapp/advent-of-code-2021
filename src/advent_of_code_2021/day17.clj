;# [Day 17](https://adventofcode.com/2021/day/17)

(ns advent-of-code-2021.day17
  (:require [clojure.java.io :as io]
            [clojure.set :as sets]
            [clojure.string :as string]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]))


; Start with functions to simulate the behavior described.

(defn next-vx
  [vx]
  (if (= 0 vx) vx (+ vx (if (< vx 1) 1 -1))))


(defn next-velocity
  [[vx vy]]
  [(next-vx vx) (dec vy)])


(defn vec+
  [[x0 y0] [x1 y1]]
  [(+ x0 x1) (+ y0 y1)])


(defn init
  [iv]
  {:vel iv
   :pos [0 0]})


(defn step
  [{:keys [vel pos] :as state}]
  {:pos (vec+ pos vel)
   :vel (next-velocity vel)})

; No need to write a function that parses just copy to the representation used here.

; target area: x=48..70, y=-189..-148
(def real-target
  {:min-x 48
   :max-x 70
   :min-y -189
   :max-y -148})


; target area: x=20..30, y=-10..-5
(def test-target
  {:min-x 20
   :max-x 30
   :min-y -10
   :max-y -5})

; Simple predicate to check if a probe state is within the target.
(defn in-target?
  [{:keys [min-x max-x min-y max-y]} {[x y] :pos}]
  (and (<= min-x x max-x)
       (<= min-y y max-y)))


; Since it doesn't matter where the probe goes after it passes the target there
; isn't any point in simulating that portion of it's trajectory. This function
; returns true once the prob is past the target. In the test case and the real 
; case the target is always below and to the right of the starting point.
(defn past-target?
  [{:keys [min-x max-x min-y max-y]} {[x y] :pos}]
  (or (< max-x x)
      (< y min-y)))

; Like take-while except that it takes until `(f x)` returns true and it
; includes `x` in the output.
(defn take-until
  [f [x & xs]]
  (lazy-seq
    (if (f x)
      [x]
      (cons x (take-until f xs)))))

; Simulates a shot of the prob. Returns all it's states from the start with
; the given initial velocity. Simulation stops once the prob is "past" the
; target.
(defn shoot
  [target iv]
  (take-until #(past-target? target %)
              (iterate step (init iv))))


; ## Part 1
; Since drag causes the prob to always causes the x position to stabilize at
; there is some minimum initial x velocity or the prob won't ever reach the 
; target in the x direction. Since the probe can skip over the target there is
; also an maximum x velocity that is the same as the target's max x. 
; The following function finds the all the valid initial x velocities between
; 1 and that max.

(defn find-valid-ivxs
  [{:keys [min-x max-x] :as target}]
  (into []
        (filter (fn [ivx]
                  (->> (iterate step (init [ivx 0]))
                       (take-until
                         (fn [{[x y] :pos [vx vy] :vel}]
                           (or (= 0 vx)
                               (< max-x x))))
                       (some (fn [{[x y] :pos}]
                               (<= min-x x max-x))))))
        (range 1 (inc max-x))))

(find-valid-ivxs test-target)
(find-valid-ivxs real-target)


; Like the maximum x velocity the minimum y velocity can't be lower (or higher
; in the negative direction) than the target's min y. I'm sure there is a way
; to figure out what the max initial y velocity could be but just picking 500
; works in both the test case and the real case. The following function takes
; a target and an initial x velocity and finds which initial y velocities will
; hit the target. It also returns the trajectory the probe took so the maximum
; y position can be found.

(defn find-valid-ivys-for-ivx
  [target ivx]
  (into []
        (comp (map (fn [ivy]
                     [[ivx ivy] (shoot target [ivx ivy])]))
              (filter (fn [[iv trajectory]]
                        (some #(in-target? target %) trajectory))))
        (range (:min-y target) 500)))


; If this were actual physics the minimum x velocity would provide the 
; trajectory that reaches that max y position. So let's try that here.
(defn max-height
  [trajectory]
  (reduce max (map #(get-in % [:pos 1]) trajectory))) 


(defn solve-part1
  [target]
  (->> (find-valid-ivxs target)
       (reduce min)
       (find-valid-ivys-for-ivx target)
       (map (fn [[iv trajectory]]
              [iv (max-height trajectory)]))
       (sort-by second)
       (last)))

(solve-part1 test-target)
(solve-part1 real-target)


; ## Part 2
; To find all possible initial velocity vectors one way would be to find the 
; min/max x/y velocities, test them all, and count which one's hit the target.
; Since I still don't know the maximum y velocity I'm just going to guess 500
; and see what happens.

(defn solve-part2
  [target]
  (->> (find-valid-ivxs target)
       (mapcat #(find-valid-ivys-for-ivx target %))
       (count)))

(solve-part2 test-target)
(solve-part2 real-target)


; I think I got lucky on the max initial y velocity thing.


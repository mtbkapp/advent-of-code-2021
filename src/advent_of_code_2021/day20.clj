;# [Day 20](https://adventofcode.com/2021/day/20)

(ns advent-of-code-2021.day20
  (:require [clojure.java.io :as io]
            [clojure.set :as sets]
            [clojure.string :as string]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]))


(def real-input (slurp (io/resource "day20.txt")))

(def test-input
  "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###")


; Read the input into a few sets. First the algorithm is read into a set where
; each element represents the offset of a `\#` character. The initial image
; is read into another set where each element are the coordinates of a lit
; pixel. Because the image is actually infinitely there needs to be a way to 
; record if those pixels are lit or not. The pixels that are not within the
; the image are only ever all lit or not all lit so the state is represented 
; in the key `:enhance`. The bounds of the image are also represented. 
(defn read-input
  [input]
  (let [split (string/split input #"\n\n")
        alg (into #{}
                  (comp (map-indexed
                          (fn [i x]
                            [i (= \# x)]))
                        (filter (comp true? second))
                        (map first))
                  (first split))
        img (into #{}
                  (comp (map-indexed
                          (fn [y row]
                            (map-indexed (fn [x col]
                                           [[x y] (= \# col)])
                                         row)))
                        (mapcat identity)
                        (filter (comp true? second))
                        (map first))
                  (string/split-lines (second split)))
        xs (map first img)
        ys (map second img)]
    {:alg alg
     :img img
     :expanse false
     :min-x (reduce min xs)
     :max-x (reduce max xs)
     :min-y (reduce min ys)
     :max-y (reduce max ys)}))


; I've split the infinite image into two parts the image or interesting pixels
; and the rest which I've called the "expanse". Something like what is below. 
; `E` and `n` denotes and expanse pixel and `i` is an image pixel. All the
; pixels labeled `n` and `i` are considered in each Enhance! step. Each step
; ends by expanding the `n` pixel ring "out" by one pixel and turning each
; previously `n` pixel into an `i` pixel.
;
; <pre>
; EEEEEEE
; EnnnnnE
; EniiinE
; EniiinE
; EniiinE
; EnnnnnE
; EEEEEEE
; </pre>
; 


; Turns the image back into the ascii art as they appear in the puzzle and
; puzzle input. 
(defn render-stdout
  [{:keys [img min-x max-x min-y max-y]}]
  (doseq [y (range min-y (inc max-y))]
    (doseq [x (range min-x (inc max-x))]
      (print (if (contains? img [x y]) \# \.)))
    (println)))


; Returns true if the point [x y] is in the infinite expanse (not in the image) 
(defn expanse?
  [{:keys [min-x max-x min-y max-y]} [x y]]
  (or (< x min-x)
      (< max-x x)
      (< y min-y)
      (< max-y y)))


; Given a sequence of booleans returns a binary number if each boolean represents
; a bit and the sequence of bits is turned into a long int.
; `(true false true) => 0b101 => 5` 
(defn bool-seq->long
  [bs]
  (Long/valueOf (apply str (map #(if % 1 0) bs)) 2))


; Given the state of the image and a pixel returns the offset into the 
; algorithm to look for that pixel's next state--lit or not lit. Accounts for
; the pixel's coordinates pointing into the expanse.
(defn offset-at
  [{:keys [img expanse] :as state} [x y]]
  (bool-seq->long
    (for [dy [-1 0 1]
          dx [-1 0 1]
          :let [p [(+ x dx) (+ y dy)]]]
      (if (expanse? state p)
        expanse
        (contains? img p)))))


; Given the state of the image returns the next state by applying the algorithm 
; for each pixel in the image and 1 pixel around the edge of the image. All
; pixels in the expanse change in the same way except for those that border the
; existing image. The expanse state transition is modeled in a different
; function.
(defn next-img 
  [{:keys [alg min-x max-x min-y max-y] :as state}]
  (assoc state
         :img
         (into #{}
               (filter #(contains? alg (offset-at state %)))
               (for [y (range (dec min-y) (+ max-y 2))
                     x (range (dec min-x) (+ max-x 2))]
                 [x y]))))


; If the expanse is all not lit then every pixel there should be `alg[0]` if
; they are all lit (like after step 1 of my puzzle input) then the offset is
; `0b111111111 = 511` so every pixel should be turned to `alg[511]`.
(defn next-expanse
  [{:keys [alg expanse] :as state}]
  (assoc state :expanse (contains? alg (if expanse 511 0))))


; Expands the bounds of the img.
(defn expand-bounds
  [state]
  (-> state
      (update :min-x dec)
      (update :max-x inc)
      (update :min-y dec)
      (update :max-y inc)))


; A full step transitions all the parts of the state:
; 1. `img` - the set of lit pixel coordinates
; 2. `alg` - the algorithm, stays constant
; 3. bounds - expands by 1 pixel in each direction 
; 4. `expanse` - if the pixels in the expanse are lit or not.
(defn step
  [state]
  (-> state next-img next-expanse expand-bounds))


; Enhance! n steps.
(defn enhance
  [input n]
  (->> (iterate step input)
       (drop n)
       (first)))


(defn solve-part1
  [input]
  (count (:img (enhance input 2))))


(solve-part1 (read-input test-input))
(solve-part1 (read-input real-input))


(defn solve-part2
  [input]
  (count (:img (enhance input 50))))


(solve-part2 (read-input test-input))
(solve-part2 (read-input real-input))


; I didn't find the image that interesting.

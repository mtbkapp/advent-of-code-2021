;# [Day 13](https://adventofcode.com/2021/day/13)

(ns advent-of-code-2021.day13
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]))


(def real-input (slurp (io/resource "day13.txt")))


(def test-input
  "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5")


(defn read-input
  [input]
  (let [[dots folds] (string/split input #"\n\n")]
    {:dots (into #{}
                 (map (fn [line]
                        (let [[x y] (string/split line #",")]
                          [(Long/valueOf x) (Long/valueOf y)])))
                 (string/split-lines dots))
     :folds (map (fn [line]
                   (let [[dir location] (-> line
                                            (string/replace "fold along " "")
                                            (string/split #"=")) ]
                     [(keyword dir) (Long/valueOf location)]))
                 (string/split-lines folds))}))


(read-input test-input)


(defn flip-up
  [[x y] loc]
  [x (- loc (- y loc))])


(defn flip-left
  [[x y] loc]
  [(- loc (- x loc)) y])


(defn partition-dots 
  [f xs]
  (reduce (fn [parts dot]
            (update parts (f dot) conj dot))
          {true #{} false #{}}
          xs))


(defn fold
  [dots dir loc]
  (let [parted (partition-dots (if (= :x dir)
                                 (fn [[x y]]
                                   (< x loc))
                                 (fn [[x y]]
                                   (< y loc)))
                               dots)]
    (into (get parted true)
          (map (if (= :x dir)
                 #(flip-left % loc)
                 #(flip-up % loc)))
          (get parted false))))


(defn solve-part1
  [{dots :dots [[dir loc]] :folds}]
  (count (fold dots dir loc)))


(solve-part1 (read-input test-input))
(solve-part1 (read-input real-input))


(defn solve-part2
  [{:keys [dots folds]}]
  (reduce (fn [ds [dir loc]]
            (fold ds dir loc))
          dots
          folds))


(defn render-stdout
  [dots]
  (let [xs (map first dots)
        ys (map second dots)
        min-x (apply min xs)
        max-x (apply max xs)
        min-y (apply min ys)
        max-y (apply max ys)]
    (doseq [y (range min-y (inc max-y))]
      (doseq [x (range min-x (inc max-x))]
        (if (contains? dots [x y])
          (print "#")
          (print " ")))
      (println))))


(defn render-clerk
  [dots]
  (let [xs (map first dots)
        ys (map second dots)
        min-x (apply min xs)
        max-x (apply max xs)
        min-y (apply min ys)
        max-y (apply max ys)]
    (clerk/table
      (mapv (fn [y]
              (mapv (fn [x]
                      (if (contains? dots [x y]) "🎄" " "))
                    (range min-x (inc max-x))))
            (range min-y (inc max-y))))))


(render-clerk (solve-part2 (read-input test-input)))
(render-clerk (solve-part2 (read-input real-input)))
(render-stdout (solve-part2 (read-input test-input)))
(render-stdout (solve-part2 (read-input real-input)))


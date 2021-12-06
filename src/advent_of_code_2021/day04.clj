;# [Day 04](https://adventofcode.com/2021/day/4) 

(ns advent-of-code-2021.day04
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]))


; read the input into strings
(def real-input (slurp (io/resource "day04.txt")))

(def test-input "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7")


; Read the list of numbers into a vector/stack. 

; Setup another vector/stack for the called numbers so the last number called
; is easily accessible

; And read each board into a nested vectors so each cell can be accessed by
; `(get-in board [row col])`. A cell is another vector of the form
; `[num marked?]`

; The collection of boards are put into a set so they can be removed easily in 
; the solution for part 2.
(defn read-board
  [board]
  (mapv (fn [line]
          (mapv (fn [x]
                  [(Long/valueOf x) false])
                (re-seq #"\d{1,2}" line))) 
        (string/split-lines board)))

(defn read-input
  [input]
  (let [[nums & boards] (string/split input #"\n\n")]
    {:nums (->> (string/split nums #",")
                (reverse)
                (mapv #(Long/valueOf %)))
     :called []
     :boards (into #{}
                   (map read-board)
                   boards)}))


; Given a board and a number to mark this function visits every cell and marks
; appropriate cells.
(defn mark-board
  [board n]
  (reduce (fn [nb [col row]]
            (update-in nb
                       [row col]
                       (fn [[bn :as cell]]
                         (if (= n bn)
                           [bn true]
                           cell))))
          board
          (for [col (range 5)
                row (range 5)]
            [col row])))


; A vector of column coordinates.
(def cols
  (mapv (fn [col]
          (mapv #(vector col %) (range 0 5)))
        (range 0 5)))


; A vector of row coordinates.
(def rows
  (mapv (fn [row]
          (mapv #(vector % row) (range 0 5)))
        (range 0 5)))


; Determine if a board is a winner by checking each row and column.
(defn winner?
  [board]
  (or (some (fn [col]
              (every? #(second (get-in board %)) col))
            cols)
      (some (fn [row]
              (every? #(second (get-in board %)) row))
            rows)))

; Execute one step of the bingo game by popping a number from the nums stack
; and pushing it onto the called stack and marking all the boards.
(defn step
  [{:keys [nums called boards] :as s}]
  (let [n (peek nums)]
    {:nums (pop nums)
     :called (conj called n)
     :boards (into #{}
                   (map #(mark-board % n))
                   boards)}))


; calculate the score of a winning board
(defn score
  [{:keys [called] :as state} winner]
  (* (peek called)
     (transduce (comp (mapcat identity)
                      (remove second)
                      (map first))
                +
                0
                winner)))


; find all the winners
(defn find-winners
  [{:keys [boards] :as state}]
  (filter winner? boards))


(defn solve-part1
  [state]
  (let [[winner] (find-winners state)]
    (if (some? winner)
      (score state winner)
      (recur (step state)))))


(solve-part1 (read-input test-input))
(solve-part1 (read-input real-input))


(defn solve-part2
  [{:keys [boards called] :as state}]
  (let [winners (find-winners state)]
    (if (empty? winners)
      (recur (step state))
      (if (= 1 (count boards))
        (score state (first winners))
        (recur (step (reduce (fn [state winner]
                               (update state :boards disj winner))
                             state
                             winners)))))))


(solve-part2 (read-input test-input))
(solve-part2 (read-input real-input))

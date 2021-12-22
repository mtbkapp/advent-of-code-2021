;# [Day 18](https://adventofcode.com/2021/day/18)

(ns advent-of-code-2021.day18
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as sets]
            [clojure.string :as string]
            [clojure.zip :as zip]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]))

; Since Snailfish numbers are just nested vectors of integers I used Clojure
; Zippers to navigate around them. It turns out finding the next "left" and
; "right" integers is super easy with zippers since the `clojure.zip/next` and
; `clojure.zip/prev` can just be applied repeatedly and do that correct thing
; when needing to move up/down the nested structure.

; Since splitting is easier than exploding I started there. To find the next
; splitable location start at the root and keep traversing with `next` until
; an integer over 10 is found.

; I found some good info on how to use zippers from: [https://grishaev.me/en/clojure-zippers/](https://grishaev.me/en/clojure-zippers/) 
(defn find-first-splitable
  [z]
  (let [n (zip/node z)]
    (cond (zip/end? z) nil
          (and (int? n) (<= 10 n)) z
          :else (recur (zip/next z)))))


(defn n->pair
  [n]
  [(long (Math/floor (/ n 2.0)))
   (long (Math/ceil (/ n 2.0)))])


(defn split
  [z]
  (zip/edit z n->pair))

 
(defn split-first
  [fish-num]
  (if-let [loc (find-first-splitable (zip/vector-zip fish-num))]
    (zip/root (split loc))))


; The length of the path to a location in a zipper is the same as it's depth.
; This means that again just but calling next repeatedly it's possible to go 
; through the zipper and stop when the conditions are met for an explodable
; location 
(defn find-first-explodable
  [z]
  (cond (zip/end? z)
        nil
        (and (= 4 (count (zip/path z))) (vector? (zip/node z)))
        z
        :else
        (recur (zip/next z))))


; Starting from an explodable location find the next and previous integer
; locations to they can be updated during the explode operation.
(defn find-next-int
  [z]
  (cond (zip/end? z) nil
        (int? (zip/node z)) z
        :else (recur (zip/next z))))


(defn find-prev-int
  [z]
  (cond (nil? (zip/path z)) nil
        (int? (zip/node z)) z
        :else (recur (zip/prev z))))


; This was the trickiest part of the implementation and I'm not very happy with
; it but it does work. I'm not sure how to save or mark a location in a zipper,
; go off and do some navigating or editing and return to it. Because of this
; This implementation goes back up to root and back down the explodable location
; several times.
(defn explode
  [fish-num]
  (if-let [exz (find-first-explodable (zip/vector-zip fish-num))]
    (let [[L R] (zip/node exz)
          left (find-prev-int exz)
          exz (if (nil? left)
                exz
                (-> (zip/edit left + L)
                    (zip/root)
                    (zip/vector-zip)
                    (find-first-explodable)))
          ; have to move past the numbers in the vector at the explodable location  
          right (find-next-int (-> exz zip/next zip/next zip/next))
          exz (if (nil? right)
                exz
                (-> (zip/edit right + R)
                    (zip/root)
                    (zip/vector-zip)
                    (find-first-explodable)))
          with-zero (zip/replace exz 0)]
      (zip/root with-zero))))


; To ensure the right order of operations in the reduction the `explode` and
; `split-first` functions return nil if they didn't make any changes and the
; new snailfish number if they did. They also only operate on the first possible
; location.
(defn reduce-step
  [fish-num]
  (if-let [exploded (explode fish-num)]
    exploded
    (if-let [splitted (split-first fish-num)]
      splitted
      fish-num)))

; To add two snailfish numbers just put them together in a vector and then keep
; reducing until no changes are made.
(defn add-fish-nums
  [fn0 fn1]
  (loop [last-fish-num nil fish-num [fn0 fn1]]
    (if (= last-fish-num fish-num)
      fish-num
      (recur fish-num (reduce-step fish-num)))))


; Just simple recursion here no need for zippers. 
(defn mag
  [fish-num]
  (if (vector? fish-num)
    (+ (* 3 (mag (first fish-num)))
       (* 2 (mag (second fish-num))))
    fish-num))


(def test-input
  "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]")


(def real-input (slurp (io/resource "day18.txt")))


; It turns out that each line is valid EDN/Clojure.
(defn read-input
  [input]
  (map #(edn/read-string %)
       (string/split-lines input)))


; ## Part 1
; Just reduce using the addition function already figured out.
(defn solve-part1
  [input]
  (mag (reduce add-fish-nums input)))


(solve-part1 (read-input test-input))
(solve-part1 (read-input real-input))


; ## Part 2
; Brute force, cuz it's fast enough. I suspect it would be much faster if the
; explode step didn't have the traverse from root more than once.
(defn solve-part2
  [input]
  (reduce (fn [max-mag [n0 n1]]
            (max max-mag (mag (add-fish-nums n0 n1))))
          -1
          (for [n0 input n1 input :when (not= n0 n1)]
            [n0 n1])))


(solve-part2 (read-input test-input))
(solve-part2 (read-input real-input))


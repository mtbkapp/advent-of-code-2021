;# [Day 18](https://adventofcode.com/2021/day/18)

(ns advent-of-code-2021.day18
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as sets]
            [clojure.string :as string]
            [clojure.zip :as zip]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]))


(def real-input (slurp (io/resource "day18.txt")))


(defn n->pair
  [n]
  [(long (Math/floor (/ n 2.0)))
   (long (Math/ceil (/ n 2.0)))])


(defn find-first-splitable
  [z]
  (let [n (zip/node z)]
    (cond (zip/end? z) nil
          (and (int? n) (<= 10 n)) z
          :else (recur (zip/next z)))))


(defn split
  [z]
  (zip/edit z n->pair))


(defn split-first
  [fish-num]
  (if-let [loc (find-first-splitable (zip/vector-zip fish-num))]
    (zip/root (split loc))))


(defn find-first-explodable
  [z]
  (cond (zip/end? z)
        nil
        (and (= 4 (count (zip/path z))) (vector? (zip/node z)))  ; Assumption, path length = depth 
        z
        :else
        (recur (zip/next z))))


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


(defn explode
  [fish-num]
  ; not sure how to mark some location and then return to it after an edit
  (if-let [exz (find-first-explodable (zip/vector-zip fish-num))]
    (let [[L R] (zip/node exz)
          left (find-prev-int exz)
          exz (if (nil? left)
                exz
                (-> (zip/edit left + L)
                    (zip/root)
                    (zip/vector-zip)
                    (find-first-explodable)))
          right (find-next-int (-> exz zip/next zip/next zip/next)) ; have to move past the numbers in the vector
          exz (if (nil? right)
                exz
                (-> (zip/edit right + R)
                    (zip/root)
                    (zip/vector-zip)
                    (find-first-explodable)))
          with-zero (zip/replace exz 0)]
      (zip/root with-zero))))


(defn reduce-step
  [fish-num]
  (if-let [exploded (explode fish-num)]
    exploded
    (if-let [splitted (split-first fish-num)]
      splitted
      fish-num)))


(defn add-fish-nums
  [fn0 fn1]
  (loop [last-fish-num nil fish-num [fn0 fn1]]
    (if (= last-fish-num fish-num)
      fish-num
      (recur fish-num (reduce-step fish-num)))))


#_(try 
    (= [[[[0,7],4],[[7,8],[6,0]]],[8,1]]
       (add-fish-nums [[[[4,3],4],4],[7,[[8,4],9]]] [1,1]))
    (catch Exception ex
      (clojure.pprint/pprint ex)))


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


(defn read-input
  [input]
  (map #(edn/read-string %)
       (string/split-lines input)))


(defn solve-part1
  [input]
  (mag (reduce add-fish-nums input)))


(solve-part1 (read-input test-input))
(solve-part1 (read-input real-input))


(defn solve-part2
  [input]
  (reduce (fn [max-mag [n0 n1]]
            (max max-mag (mag (add-fish-nums n0 n1))))
          -1
          (for [n0 input n1 input :when (not= n0 n1)]
            [n0 n1])))

(solve-part2 (read-input test-input))



; path length = depth?
(comment
  (let [init (zip/vector-zip (clojure.edn/read-string "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"))]
    (loop [zipper init]
      (when (not (zip/end? zipper))
        (prn {:path-len (count (zip/path zipper))
              :path-nil? (nil? (zip/path zipper))
              :node (zip/node zipper)})
        (recur (zip/next zipper)))))


  )

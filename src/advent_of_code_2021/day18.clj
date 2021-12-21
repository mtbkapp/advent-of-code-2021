;# [Day 18](https://adventofcode.com/2021/day/18)

(ns advent-of-code-2021.day18
  (:require [clojure.java.io :as io]
            [clojure.set :as sets]
            [clojure.string :as string]
            [clojure.zip :as zip]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]))


(def real-input (slurp (io/resource "day18.txt")))


(-> (clojure.edn/read-string "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")
    zip/vector-zip
    zip/down
    zip/up
    zip/next
    zip/next
    zip/next
    zip/next
    zip/next
    zip/next
    zip/next
    zip/next
    zip/path
    count
    )

#_(-> (zip/vector-zip (clojure.edn/read-string "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"))
      zip/down
      zip/down
      zip/down
      zip/down
      explode
      zip/node
      )


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
  (z/edit z n->pair))


(defn split-first
  [fish-num]
  (if-let [loc (find-first-splitable (zip/vector-zip fish-num))]
    (zip/root (split loc))))


(defn find-first-explodable
  [z]
  (cond (zip/end? z) nil
        (= 4 (count (zip/path z))) z ; Assumption, path length = depth
        :else (recur (zip/next z))))


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


(explode [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]])
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
          right (find-next-int (-> exz zip/next zip/next zip/next)) ; have to move past the numbers in the vector
          exz (if (nil? right)
                exz
                (-> (zip/edit right + R)
                    (zip/root)
                    (zip/vector-zip)
                    (find-first-explodable)))
          with-zero (zip/replace exz 0)]
      (zip/root with-zero))))

; WOULD THIS ALL BE MUCH EASIER WITH MUTABLE LINKED LISTS OR PAIRS OR SOMETHING!

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

#_(add-fish-nums [[[[4,3],4],4],[7,[[8,4],9]]] [1,1])



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

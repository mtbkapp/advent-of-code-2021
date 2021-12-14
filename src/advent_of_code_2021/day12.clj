;# [Day 12](https://adventofcode.com/2021/day/12)

(ns advent-of-code-2021.day12
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]))


(def real-input (slurp (io/resource "day12.txt")))


; Works like `conj` except that in the case that the first arg is nil it will
; replace it with `#{}`. So `(conj-set nil :A) => #{:A}` instead of `(:A)`. 
(def conj-set (fnil conj #{}))


; The graph is stored as a mapping from a room to a set of adjacent rooms. This 
; function updates the graph with a given undirected edge taking care that
; `:start` is never in any room's adjacent set and `:end` only appears in
; adjacent sets.
(defn add-edge
  [graph [a b]]
  (cond-> graph
    (and (not= :end b) (not= :start a)) (update b conj-set a)
    (and (not= :end a) (not= :start b)) (update a conj-set b)))


(defn read-input
  [input]
  (transduce
    (comp (map #(string/split % #"-"))
          (map #(mapv keyword %)))
    (completing add-edge)
    {}
    (string/split-lines input)))


; ## Part 1

(defn big?
  [room]
  (= (str room) (string/upper-case (str room))))


(defn small?
  [room]
  (or (= room :start)
      (= room :end)
      (not (big? room))))

; The main part of the algorithm. This recursive function returns all the paths
; to end given an input graph, the path traveled so far, and room currently
; being visited.
; * The base case is where `room` is the `:end` room so it just returns a single
;   path with :end added to the end of the path traveled so far.
; * The recursive case takes the rooms adjacent to the given room and removing
;   small rooms which have already been visited. Each of these rooms is
;   considered independently by making a recursive call with the path updated
;   with the given room and the current room being each adjacent room. When
;   the paths are returned from each recursive call they are combined and
;   returned.
(defn paths-from
  [graph path-to room]
  (if (= :end room)
    [(conj path-to room)]
    (into []
          (comp (remove (into #{} (filter small?) path-to))
                (mapcat (partial paths-from graph (conj path-to room))))
          (get graph room))))


(defn solve-part1
  [graph]
  (count (paths-from graph [] :start)))


(def test-input1
  "start-A
start-b
A-c
A-b
b-d
A-end
b-end")


(solve-part1 (read-input test-input1))


(def test-input2
  "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc")


(solve-part1 (read-input test-input2))


(def test-input3
  "fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW")


(solve-part1 (read-input test-input3))

; Ok, ok solve for the real graph and get a gold star!
(solve-part1 (read-input real-input))

; ## Part 2 - now the sub can visit a single small room twice.


(defn visited-two-small? 
  [path]
  (->> (frequencies path)
       (filter (fn [[room visit-count]]
                 (and (small? room)
                      (= 2 visit-count))))
       (count)
       (= 1)))


(defn adjacent
  [graph path room]
  (let [adj (get graph room)]
    (if (visited-two-small? path)
      (remove (into #{} (filter small?) path) adj)
      adj)))


(defn paths-from2
  [graph path-to room]
  (let [new-path (conj path-to room)]
    (if (= :end room)
      [new-path]
      (mapcat (fn [adj]
                (paths-from2 graph new-path adj))
              (adjacent graph new-path room)))))


(defn solve-part2
  [graph]
  (count (paths-from2 graph [] :start)))


(solve-part2 (read-input test-input1))
(solve-part2 (read-input real-input))


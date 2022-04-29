;# [Day 23](https://adventofcode.com/2021/day/23)

(ns advent-of-code-2021.day23
  (:require [clojure.data.priority-map :as pm]
            [clojure.set :as sets]
            [clojure.spec.alpha :as spec]
            [clojure.string :as string]
            [clojure.test :refer :all]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]))


(set! *warn-on-reflection* true)

; ## My interpretation of the rules:
; 1. can't *stop* on a space immediately outside a room
; 2. can't enter a room unless it is empty or contains the other correct amphipod 
; 3. can stop in hallway but it'll be stuck there until it can move into its correct room. 
;
; ## State representation
;   0000000000111
;   0123456789012
; 0 #############
; 1 #...........#
; 2 ###A#B#C#D###
; 3 ..#A#B#C#D#..
; 4 ..#########..
; 
; ## Solution
; The state space can be modeled as a weighted directed graph. Each state has 
; adjacent states where an amphipod moves to another valid position. The edges 
; are weighted by the cost of the move. Then 
; [Dijstra's Algorithm](https://en.wikipedia.org/wiki/Dijkstra's_algorithm)
; can be used to find the minimum cost to a final state. Since there are 16
; possible final states (at least in part 1, 256 in part 2) the min cost to 
; each has to be considered. However not all are necessarily reachable.
; 
; The JDK's priority queue, `java.util.PriorityQueue` doesn't have a method to
; change the priority of an object. Although it is possible to implement 
; Dijkstra's algorithm with a priority queue without that feature I chose to
; use [clojure.data.priority-map](https://github.com/clojure/data.priority-map)
; which fits into Clojure easier can priorities can be updated.   
;
; The rules imply that an amphipod must either end in one of the valid hallway 
; positions or in its home cave there are only a few possible routes it can 
; take:
; 
; 1. start cave -> hallway -> cave
; 2. start cave -> end cave
;
; So that simplifies finding paths a little. I implemented functions to find
; paths from the start cave to a hallway location, a hallway location to the
; a destination cave, and finally from a start cave to a destination cave.
;
; To find adjacent states:
; 1. find amphipods that aren't in their destination cave.
; 2. for each of those amphipods find the paths that could be taken ignore
;    the fact that other amphipods could be in the way. The walls can be 
;    ignored here because they are accounted for in the path functions.
; 3. remove the paths that have another amphipod in the way
; 4. for each path calculate the cost and the state
;
; It turns out that it's pretty slow but it works. I read that others have
; solved this with the A* algorithm to make things faster.

(def test-input
  "#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########")


(def real-input
  "#############
#...........#
###B#A#B#C###
  #C#D#D#A#
  #########")


(defn init-state
  [input]
  (transduce
    (comp (map-indexed
            (fn [y line]
              (map-indexed (fn [x c]
                             [[x y] c])
                           line)))
          (mapcat identity)) 
    (completing
      (fn [acc [pos c]]
        (if (contains? #{\A \B \C \D} c)
          (assoc acc
                 (if (nil? (get acc (keyword (str c 1))))
                   (keyword (str c 1))
                   (keyword (str c 2)))
                 pos)
          acc)))
    {:A1 nil :A2 nil :B1 nil :B2 nil :C1 nil :C2 nil :D1 nil :D2 nil}
    (string/split-lines input)))


(init-state test-input)


(def amphipod-type->cost
  {:A 1
   :B 10
   :C 100
   :D 1000})


(def own-cave-x
  {:A 3
   :B 5
   :C 7
   :D 9})


(def hallway-stops
  (into #{}
        (map #(vector % 1))
        #{1 2 4 6 8 10 11}))


(defn cave?
  [[x y]]
  (< 1 y))


(defn hall?
  [[x y]]
  (= y 1))


(defn find-path-cave-to-hall 
  [[sx sy :as start] [ex ey :as end]]
  (if (= start end)
    [end]
    (let [n (cond (< 1 sy) [sx (dec sy)]
                  (< sx ex) [(inc sx) sy]
                  :else [(dec sx) sy])]
      (cons start (find-path-cave-to-hall n end)))))


(defn find-path-hall-to-cave
  [[sx sy :as start] [ex ey :as end]]
  (if (= start end)
    [end]
    (let [n (if (= sx ex)
              [sx (inc sy)]
              (if (< sx ex)
                [(inc sx) sy]
                [(dec sx) sy]))]
      (cons start (find-path-hall-to-cave n end)))))


(defn find-path-cave-to-cave
  [[sx sy :as start] end]
  (let [hall [sx 1]]
    (concat (find-path-cave-to-hall start hall)
            (rest (find-path-hall-to-cave hall end)))))


(defn amphipod-positions
  [state]
  (set (vals state)))


(defn amphipod-type
  [amphipod]
  (-> amphipod name first str keyword))
 

(defn path-cost
  [amphipod path]
  (* (amphipod-type->cost (amphipod-type amphipod))
     (dec (count path))))


(defn next-free-cave-pos
  [state amphipod]
  (let [ps (amphipod-positions state) 
        x (own-cave-x (amphipod-type amphipod))]
    (some #(if (not (contains? ps %)) %) [[x 3] [x 2]])))


(defn potential-paths
  [state amphipod]
  (let [[x y :as pos] (get state amphipod)
        dest-cave (next-free-cave-pos state amphipod)]
    (if (cave? pos)
      (cond-> (map #(find-path-cave-to-hall pos %) hallway-stops)
        (some? dest-cave)
        (conj (find-path-cave-to-cave pos dest-cave)))
      (if (some? dest-cave)
        [(find-path-hall-to-cave pos dest-cave)]))))


(defn next-moves
  [state amphipod]
  (let [curr-ps (amphipod-positions state)]
    (into []
          (comp (filter #(empty? (sets/intersection curr-ps (set (rest %)))))
                (map (juxt (partial path-cost amphipod) last)))
          (potential-paths state amphipod))))


(defn like-amphipod
  [amphipod]
  (keyword (str (name (amphipod-type amphipod))
                (if (= \1 (second (name amphipod)))
                  \2
                  \1))))


(defn amphipod-home?
  [state amphipod]
  (let [[x y :as pos] (get state amphipod)
        cave-x (own-cave-x (amphipod-type amphipod))]
    (and (= x cave-x)
         (or (= y 3)
             (and (= y 2)
                  (= [cave-x 3]
                     (get state (like-amphipod amphipod))))))))


(defn next-states
  [state]
  (into []
        (comp (remove (partial amphipod-home? state))
              (mapcat (fn [amphipod]
                        (map (fn [[cost new-pos]]
                               [cost (assoc state amphipod new-pos)])
                             (next-moves state amphipod)))))
        (keys state)))


(defn dijkstra
  [start adjacent]
  (loop [pq (pm/priority-map start 0)
         dist {start 0}]
    (if (empty? pq)
      dist
      (let [[u dist-u] (peek pq)
            {:keys [next-pq next-dist]}
            (reduce (fn [acc [cost v]]
                      (let [alt (+ dist-u cost)]
                        (if (< alt (get-in acc [:next-dist v] Long/MAX_VALUE))
                          (-> acc
                              (assoc-in [:next-pq v] alt)
                              (assoc-in [:next-dist v] alt))
                          acc)))
                    {:next-pq (pop pq)
                     :next-dist dist}
                    (remove (partial contains? dist)
                            (adjacent u)))]
        (recur next-pq next-dist)))))


(defn every-amphipod-home?
  [state]
  (every? (partial amphipod-home? state) (keys state)))


(defn find-min-final
  [dijkstra-result]
  (reduce (fn [min-cost [state cost]]
            (if (every-amphipod-home? state)
              (min min-cost cost)
              min-cost))
          Long/MAX_VALUE
          dijkstra-result))


(defn solve-part-1
  [input]
  (-> (dijkstra (init-state input) next-states)
      (find-min-final)))

(solve-part-1 test-input)
(solve-part-1 real-input)

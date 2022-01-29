;# [Day 21](https://adventofcode.com/2121/day/21)

(ns advent-of-code-2021.day21
  (:require [clojure.java.io :as io]
            [clojure.set :as sets]
            [clojure.string :as string]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]))


(defn init
  [p1-start p2-start]
  {:die 1
   :rounds 0
   :p1 {:score 0 :pos p1-start} 
   :p2 {:score 0 :pos p2-start}})


(defn next-pos
  [pos d1 d2 d3]
  (inc (mod (dec (+ pos d1 d2 d3)) 10)))

(next-pos 5 1 2 3)
(next-pos 8 4 5 6)


(defn next-die
  [die]
  (if (= die 100)
    1
    (inc die)))


#_(round (init 4 8))
#_(clojure.pprint/pprint (take 20 (iterate round (init 4 8))))
(defn round
  [{:keys [die p1 p2] :as state}]
  (let [[d1 d2 d3 d4 d5 d6] (take 6 (iterate next-die die)) 
        p1-next-pos (next-pos (:pos p1) d1 d2 d3)
        p2-next-pos (next-pos (:pos p2) d4 d5 d6)]
    (-> state
        (update :rounds inc)
        (assoc :die (next-die d6))
        (assoc-in [:p1 :pos] p1-next-pos)
        (update-in [:p1 :score] + p1-next-pos)
        (assoc-in [:p2 :pos] p2-next-pos)
        (update-in [:p2 :score] + p2-next-pos))))


(defn end?
  [{:keys [p1 p2] :as state}]
  (or (<= 1000 (:score p1))
      (<= 1000 (:score p2))))


(defn finalize 
  [{:keys [p1 p2 rounds] :as state}]
  (if (< (:score p1) (:score p2))
    (assoc state
           :rolls (* rounds 6)
           :loser :p1)
    (-> state
        (update-in [:p2 :score] - (:pos p2))
        (assoc :rolls (- (* rounds 6) 3))
        (assoc :loser :p2))))


(defn game
  [p1-start p2-start]
  (->> (iterate round (init p1-start p2-start))
       (drop-while (complement end?))
       first
       finalize))


(defn solve-part1
  [p1-start p2-start]
  (let [{:keys [loser rolls] :as state} (game p1-start p2-start)]
    (* rolls (get-in state [loser :score]))))



#_(solve-part1 4 8)
#_ (try #_ (prn (solve-part1 1 3))
        (->> (iterate round (init 1 3))
             (drop-while (complement end?))
             first

             )
        (catch Exception ex
          (clojure.pprint/pprint ex)
          )
        )

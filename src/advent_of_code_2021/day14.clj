;# [Day 14](https://adventofcode.com/2021/day/14)

(ns advent-of-code-2021.day14
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]))


(def real-input (slurp (io/resource "day14.txt")))


(def test-input
  "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C")


(defn read-input
  [input]
  (let [[temp sbs] (string/split input #"\n\n")]
    {:temp temp
     :sbs (map #(string/split % #" \-\> ") (string/split-lines sbs))}))


(read-input test-input)


(defn find-pair
  [temp pair]
  (into []
        (comp (map-indexed vector)
              (filter #(= pair (apply str (second %))))
              (map first))
        (partition 2 1 temp)))


(defn insert-pairs
  [temp sbs]
  (transduce (map-indexed vector)
             (completing
               (fn [new-temp [pos-i [i c]]]
                 (let [ri (+ pos-i i)]
                   (str (subs new-temp 0 (inc ri))
                        c
                        (subs new-temp (inc ri))))))
             temp
             sbs))



#_(step (read-input test-input))
(defn step
  ([{:keys [temp sbs]}] (step temp sbs))
  ([temp sbs]
   {:temp
    (insert-pairs temp
                  (sort-by first
                           (mapcat (fn [[pair c]]
                                     (map #(vector % c)  (find-pair temp pair))
                                     )
                                   sbs)))
    :sbs sbs}))

(count (:temp (first (drop 10 (iterate step (read-input test-input))))))


(defn run
  [input n]
  (->> (iterate step input)
       (drop n)
       (first)))


(defn solve-part1
  [input]
  (let [{:keys [temp]} (run input 10)
        sorted (sort-by val (frequencies temp))
        [_ f1] (first sorted)
        [_ f2] (last sorted)]
    (- f2 f1)))


#_(solve-part1 (read-input test-input))
#_(solve-part1 (read-input real-input))


; After browsing around on reddit for clues and things in the #funny section
; this brute force approach won't work at all. It'll take too long and too much
; memory.

(init-pairs (read-input test-input))
(defn init-pairs
  [{:keys [temp sbs] :as input}]
  (assoc input
         :pairs
         (->> (partition 2 1 temp)
              (map #(apply str %))
              (frequencies))))

#_(:pairs (step2 (step2 (step2 (step2 (init-pairs (read-input test-input)))))))
(defn step2
  [{:keys [pairs sbs] :as state}]
  (assoc state
         :pairs
         (let [changes (->> (filter (fn [[pair sub]] (contains? pairs pair)) sbs)
                            (mapcat (fn [[pair sub]]
                                      (let [[a b] pair]
                                        [[:remove pair]
                                         [:add (str a sub)]
                                         [:add (str sub b)]])))
                            (group-by first))]
           (as-> pairs $
             (reduce (fn [pairs [_ p]]
                       (dissoc pairs p))
                     $
                     (:remove changes))
             (reduce (fn [pairs [_ p]]
                       (update pairs p (fnil inc 0)))
                     $
                     (:add changes))))))


(defn element-frequencies
  [{:keys [pairs]}] 
  (reduce (fn [freqs [pair pair-count]]
            (-> freqs
                
                ))
          {}
          pairs))


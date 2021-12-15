;# [Day 14](https://adventofcode.com/2021/day/14)

(ns advent-of-code-2021.day14
  (:require [clojure.java.io :as io]
            [clojure.set :as sets]
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


(defn read-input2
  [input]
  (let [[temp sbs] (string/split input #"\n\n")]
    (let  [ps (partition 2 1 temp)]
      {:pairs (frequencies ps) 
       :last-pair (last ps)
       :sbs (into {}
                  (comp (map #(string/split % #" \-\> "))
                        (map (fn [[[x y] [z]]]
                               [[x y] [[x z] [z y]]])))
                  (string/split-lines sbs))})))


(read-input2 test-input)


(defn next-pairs
  [pairs sbs]
  (reduce (fn [np [p c]]
            (let [[np1 np2] (get sbs p)]
              (-> np
                  (update np1 (fnil + 0) c)
                  (update np2 (fnil + 0) c))))
          {}
          pairs))


(defn next-last-pair
  [last-pair sbs]
  (second (get sbs last-pair)))


(defn step
  [{:keys [pairs last-pair sbs] :as input}]
  (assoc input
         :pairs (next-pairs pairs sbs)
         :last-pair (next-last-pair last-pair sbs)))



; don't really need to keep track of last pair, just last element. It never
; changes but isn't counted by only counting the first element of each pair
(defn count-els
  [{:keys [pairs last-pair] :as input}]
  (reduce (fn [freqs [[a b] c]]
            (update freqs a (fnil + 0) c))
          {(last last-pair) 1}
          pairs))


(defn run
  [input n]
  (->> (iterate step input)
       (drop n)
       first))


(defn most-minus-least
  [input]
  (let [counts (->> input
                   count-els
                   (sort-by val))]
    (- (val (last counts))
       (val (first counts)))))


(defn solve-part1
  [input]
  (most-minus-least (run input 10)))


(solve-part1 (read-input2 test-input))
(solve-part1 (read-input2 real-input))


(defn solve-part2
  [input]
  (most-minus-least (run input 40)))


(solve-part2 (read-input2 test-input))
(solve-part2 (read-input2 real-input))

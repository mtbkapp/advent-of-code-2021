;# [Day 08](https://adventofcode.com/2021/day/8) 

(ns advent-of-code-2021.day08
  (:require [clojure.java.io :as io]
            [clojure.set :as sets]
            [clojure.string :as string]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]))


; read the input into strings
(def real-input (slurp (io/resource "day08.txt")))


(def test-input "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")


(defn str->wire-set
  [s]
  (into #{} (map (comp keyword str)) s))

(defn read-input
  [input]
  (map (fn [line]
         (let [[unique display] (string/split line #" \| ")]
           {:unique (map str->wire-set (string/split unique #"\s"))
            :display (map str->wire-set (string/split display #"\s"))}))
       (string/split-lines input)))

(read-input test-input)


(def unique-counts #{2 4 3 7})


(defn solve-part1
  [input]
  (->> input
       (map :display)
       (mapcat #(map count %))
       (filter unique-counts)
       (count)))


(solve-part1 (read-input test-input))
(solve-part1 (read-input real-input))


; ## Part 2
; 
; ```
; AAAAA
; B   C
; B   C
; DDDDD
; E   F
; E   F
; GGGGG
; ```

(def digit->segments
  {0 #{:A :B :C :E :F :G}
   1 #{:C :F}
   2 #{:A :C :D :E :G}
   3 #{:A :C :D :F :G}
   4 #{:B :C :D :F}
   5 #{:A :B :D :F :G}
   6 #{:A :B :D :E :F :G}
   7 #{:A :C :F}
   8 #{:A :B :C :D :E :F :G}
   9 #{:A :B :C :D :F :G}})


(def wire-count->potential-digits
  (reduce (fn [r [digit segs]]
            (update r
                    (count segs)
                    (fnil conj #{})
                    digit))
          {}
          digit->segments))


(def init-segment->potential-wires 
  {:A #{:a :b :c :d :e :f :g}
   :B #{:a :b :c :d :e :f :g}
   :C #{:a :b :c :d :e :f :g}
   :D #{:a :b :c :d :e :f :g}
   :E #{:a :b :c :d :e :f :g}
   :F #{:a :b :c :d :e :f :g}
   :G #{:a :b :c :d :e :f :g}})


(defn init-pattern->possible-digits
  [pattern]
  (zipmap pattern
          (map #(wire-count->potential-digits (count %)) pattern)))



(def test-pattern
  [#{:a :c :e :d :g :f :b}
   #{:c :a :g :e :d :b}
   #{:c :e :f :a :b :d}
   #{:c :d :f :g :e :b}
   #{:c :d :f :b :e}
   #{:g :c :d :f :a}
   #{:f :b :c :a :d}
   #{:e :a :f :b}
   #{:d :a :b}
   #{:a :b}])


(init-pattern->possible-digits test-pattern)

; why doesn't the "x more..." expansion work anymore...
{#{:b :a} #{1}
 #{:b :d :a} #{7}
 #{:e :b :f :a} #{4}
 #{:e :c :b :d :f} #{3 2 5}
 #{:c :b :d :f :a} #{3 2 5}
 #{:g :c :d :f :a} #{3 2 5}
 #{:e :g :c :b :d :a} #{0 6 9}
 #{:e :g :c :b :d :f} #{0 6 9}
 #{:e :c :b :d :f :a} #{0 6 9}
 #{:e :g :c :b :d :f :a} #{8}}


(defn update-for-known-digit
  [segment->potential-wires wires digit]
  (let [segments (digit->segments digit)]
    (reduce (fn [spw [seg pot-wires]]
              (assoc spw
                     seg
                     (if (contains? segments seg)
                       (sets/intersection pot-wires wires)
                       (sets/difference pot-wires wires))))
            {}
            segment->potential-wires)))


(update-for-known-digit init-segment->potential-wires #{:a :b} 1)

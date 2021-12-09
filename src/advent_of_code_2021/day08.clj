;# [Day 08](https://adventofcode.com/2021/day/8) 

(ns advent-of-code-2021.day08
  (:require [clojure.java.io :as io]
            [clojure.set :as sets]
            [clojure.string :as string]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]
            [clojure.core.logic :as L]
            [clojure.core.logic.fd :as FD]))


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


(defn read-input
  [input]
  (map (fn [line]
         (let [[unique display] (string/split line #" \| ")]
           {:unique (string/split unique #"\s")
            :display (string/split display #"\s")}))
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

; Make each segment of the display a logic variable 
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
  {0 [:A :B :C :E :F :G]
   1 [:C :F]
   2 [:A :C :D :E :G]
   3 [:A :C :D :F :G]
   4 [:B :C :D :F]
   5 [:A :B :D :F :G]
   6 [:A :B :D :E :F :G]
   7 [:A :C :F]
   8 [:A :B :C :D :E :F :G]
   9 [:A :B :C :D :F :G]})


(def count->potential-digits
  (reduce (fn [r [digit segs]]
            (update r (count segs) conj digit))
          {}
          digit->segments))

; A recursive (and probably slow) way to build all the unique ways to order
; a set of elements.
;
; The base cases are:
;   if the input has no elments -> there are no permutations
;   if the input has 1 element -> there there is only 1 permutation
; The recursive case is:
;   Take each element in the set as the first element of a permutation and
;   concatenate it to the permutations of the rest of the elements.
;
; Example:
; Input: `#{1 2}`
;
; Output:
; | Element | Permutations of remaining |
; |---------|---------------------------|
; |`1`      | `[2]`                     |
; |`2`      | `[1]`                     |
; `#{[1 2] [2 1]}` 
;   
; Example:
; Input: `#{1 2 3}`
;
; Output:
; | Element | Permutations of remaining |
; |---------|---------------------------|
; |`1`      | `[2 3] [3 2]`             |
; |`2`      | `[1 3] [3 1]`             |
; |`3`      | `[2 1] [1 2]`             |
; ```
; #{[1 2 3] [1 3 2]
;   [2 1 3] [2 3 1]
;   [3 2 1] [3 2 1]}
; ```
;   
(defn permutations
  [xs]
  (cond (empty? xs) #{} 
        (= 1 (count xs)) #{(vec xs)}
        :else (into #{}
                    (mapcat
                      (fn [x]
                        (map #(cons x %) (permutations (disj xs x)))))
                    xs)))

(permutations #{})
(permutations #{'a})
(permutations #{'a 'b})
(count (permutations #{'a 'b 'c 'd 'e 'f 'g}))


; start with all the possibilities
;
; 

(def start
  {:A #{:a :b :c :d :e :f :g}
   :B #{:a :b :c :d :e :f :g}
   :C #{:a :b :c :d :e :f :g}
   :D #{:a :b :c :d :e :f :g}
   :E #{:a :b :c :d :e :f :g}
   :F #{:a :b :c :d :e :f :g}
   :G #{:a :b :c :d :e :f :g}})

; start with the patterns where there can only be one digit
; 
#_(require 'clojure.core.logic.fd)
(clojure.repl/dir clojure.core.logic.fd)

; lets start with "ab" which must be the digit 1
(count->potential-digits (count "ab"))

; now get the segments that make up a 1
(digit->segments 1)

; now for each of those update the starting possibilities
(let [wires (set (map (comp keyword str) "ab"))]
  (reduce (fn [P seg]

            (update P (keyword seg) sets/intersection wires)
            )
          start
          (digit->segments 1)))

(defn step
  [P digit wires]
  (let [kws (set (map (comp keyword str) wires))]
    (reduce
      (fn [P' seg]
        (update P' (keyword seg) sets/intersection kws))
      P
      (digit->segments digit))))


(-> start
    (step 1 "ab")
    ;(step 4 "eafb")
    ;(step 7 "dab")
    ;(step 8 "acedgfb")
    )

; eval'd from bottom up.
{"acedgfb" #{8}
 "cefabd" #{6 9 0}
 "cdfgeb" #{6 9 0}
 "cagedb" #{6 9 0}
 "cdfbe" #{2 3 5} ; #{5} 
 "gcdfa" #{2 3 5} ; #{2 5}, must be 2
 "fbcad" #{2 3 5} ; must be 3, cuz contains same wires as 7
 "eafb" #{4}
 "dab" #{7}
 "ab" #{1}}


; ```
; AAAAA
; B   C
; B   C
; DDDDD
; E   F
; E   F
; GGGGG
; ```


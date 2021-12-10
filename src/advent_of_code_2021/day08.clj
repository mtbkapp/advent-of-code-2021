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


(def test-pattern "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")

(read-input test-pattern)


; ### Step 1
; Map patterns to potential digits via pattern size.
; * $P_1$ has 2 wires.  
; * $P_7$ has 3 wires.
; * $P_4$ has 4 wires. 
; * $P_2$, $P_3$, and $P_5$ all have 5 wires.
; * $P_6$, $P_9$, and $P_0$ all have 6 wires.
; * $P_8$ has 7 wires.


(def count->potential-digits
  (reduce (fn [cpd [digit segs]]
            (update cpd (count segs) (fnil conj #{}) digit))
          {}
          digit->segments))


(defn init-pattern->potential-digits
  [unique-patterns]
  (reduce (fn [ppd pattern]
            (assoc ppd pattern (count->potential-digits (count pattern))))
          {}
          unique-patterns))


(def init-segment->potential-wires
  (zipmap [:A :B :C :D :E :F :G]
          (repeat #{:a :b :c :d :e :f :g})))


(defn init-state
  [{:keys [unique]}]
  {:segment->potential-wires init-segment->potential-wires
   :pattern->potential-digits (init-pattern->potential-digits unique)})


; *nextjournal.clerk renders pattern->potential-digits incorrectly*
(init-state (first (read-input test-pattern)))


; ### Step 2
; Given an initial mapping of segments to potential wires where each segment
; could be mapped to any wire update it by using the patterns that are 
; already know. Which are the $P_1$, $P_7$, and $P_4$.

(defn update-segment->potential-wires-for-known-digit
  [state pattern digit]
  (update state
          :segment->potential-wires
          #(let [digit-segs (digit->segments digit)]
             (reduce (fn [spw [seg pw]]
                       (update spw
                               seg
                               (if (contains? digit-segs seg)
                                 sets/intersection
                                 sets/difference)
                               pattern))
                     % 
                     %))))


(defn step2
  [{:keys [pattern->potential-digits] :as state}]
  (reduce (fn [state [pattern pd]]
            (if (= 1 (count pd))
              (update-segment->potential-wires-for-known-digit
                state
                pattern
                (first pd))
              state))
          state
          pattern->potential-digits))


; *nextjournal.clerk renders pattern->potential-digits incorrectly*
(step2 (init-state (first (read-input test-pattern))))


; ### Step 3
; The next step is to figure out which pattern match the digit 3.
; Because $P_3 \supset P_1$ and $P_2 \supset P_1, P_5 \supset P_1$ can not 
; also be true then only one pattern can be $P_3$ out of the patterns than can
; be 2, 3, or 5.

(defn patterns-of-size
  [pattern->potential-digits n]
  (into []
        (comp (map key)
              (filter #(= n (count %))))
        pattern->potential-digits))


(defn update-pattern->potential-digits-for-known-digit
  [state pattern digit]
  (update state
          :pattern->potential-digits
          #(reduce (fn [ppd [p pd]]
                     (assoc ppd
                            p
                            (if (= p pattern)
                              #{digit}
                              (disj pd digit))))
                   %
                   %)))


(defn step3
  [{:keys [pattern->potential-digits] :as state}]
  (let [p1 (first (patterns-of-size pattern->potential-digits 2))
        p3 (first (filter #(sets/subset? p1 %)
                          (patterns-of-size pattern->potential-digits 5)))]
    (-> state
        (update-segment->potential-wires-for-known-digit p3 3)
        (update-pattern->potential-digits-for-known-digit p3 3))))


; *nextjournal.clerk renders pattern->potential-digits incorrectly*
(-> (init-state (first (read-input test-pattern)))
    step2
    step3)


; ### Step 4
; Similar to Step 3 it turns out that $P_9 \supset P_4$ and $P_6 \supset P_4$ 
; is *not* true and $P_0 \subset P_4$ is also *not* true. This will yield any
; new info for `segment->potential-wires` but will for
; `pattern->potential-wires`.

(defn step4
  [{:keys [pattern->potential-digits] :as state}]
  (let [p4 (first (patterns-of-size pattern->potential-digits 4))
        p9 (first (filter #(sets/subset? p4 %)
                          (patterns-of-size pattern->potential-digits 6)))]
    (update-pattern->potential-digits-for-known-digit state p9 9)))


; *nextjournal.clerk renders pattern->potential-digits incorrectly*
(-> (init-state (first (read-input test-pattern)))
    step2
    step3
    step4)


(defn find-potential-patterns-for-digit
  [pattern->potential-digits d]
  (into []
        (comp (filter #(contains? (val %) d))
              (map key))
        pattern->potential-digits))


; ### Step 5
; At this point only the potential wires for segments `:C` and `:F` are
; ambiguous. So a digit who's pattern that only contains `:C` or `:F` but not 
; both. One such digit is 2. At this point the wires for `:A`, `:D`, `:E` and
; `:G` are known. So $P_2 \supset \{ w_A, w_D, w_E, w_G \}$.

(defn step5
  [{:keys [pattern->potential-digits segment->potential-wires] :as state}]
  (let [subset (sets/union (segment->potential-wires :A)
                           (segment->potential-wires :D)
                           (segment->potential-wires :E)
                           (segment->potential-wires :G))
        [p2] (filter #(sets/subset? subset %)
                     (find-potential-patterns-for-digit
                       pattern->potential-digits 2))]
    (-> state
        (update-segment->potential-wires-for-known-digit p2 2)
        (update-pattern->potential-digits-for-known-digit p2 2))))


; *nextjournal.clerk renders pattern->potential-digits incorrectly*
(-> (init-state (first (read-input test-pattern)))
    step2
    step3
    step4
    step5)


; ### Step 6
; At this point all the segments are only mapped to one wire! Finally. However
; the patterns $P_6$ and $P_0$ are still ambiguous. They can be found via by
; using `segment->potential-wires`.

(defn pattern-for-digit
  [segment->potential-wires digit]
  (transduce (map segment->potential-wires)
             sets/union
             (digit->segments digit)))


(defn step6
  [{:keys [segment->potential-wires] :as state}]
  (update-pattern->potential-digits-for-known-digit
    state
    (pattern-for-digit segment->potential-wires 6)
    6))


; *nextjournal.clerk renders pattern->potential-digits incorrectly*
(-> (init-state (first (read-input test-pattern)))
    step2
    step3
    step4
    step5
    step6)


; ### Map display pattern to digits
(defn map-display*
  [pattern->potential-digits display]
  (->> (map (comp first pattern->potential-digits) display)
       (apply str)
       (Long/valueOf)))


(defn map-display
  [{:keys [display] :as input}]
  (map-display* (-> (init-state input)
                    step2
                    step3
                    step4
                    step5
                    step6
                    :pattern->potential-digits)
                display))


; ***Success!***
(map-display (first (read-input test-pattern)))


(defn solve-part2
  [input]
  (transduce (map map-display) + input))


(solve-part2 (read-input test-input))
(solve-part2 (read-input real-input))

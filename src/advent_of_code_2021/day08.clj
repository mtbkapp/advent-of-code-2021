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



(def test-patterns
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


(init-pattern->possible-digits test-patterns)

; why doesn't the "x more..." expansion work anymore...
{#{:b :a} #{1}
 #{:b :d :a} #{7}
 #{:e :b :f :a} #{4}
 #{:c :b :d :f :a} #{3 2 5}
 #{:e :c :b :d :f} #{3 2 5}
 #{:g :c :d :f :a} #{3 2 5}
 #{:e :g :c :b :d :a} #{0 6 9}
 #{:e :g :c :b :d :f} #{0 6 9}
 #{:e :c :b :d :f :a} #{0 6 9}
 #{:e :g :c :b :d :f :a} #{8}}

; This function removes possibilities from the segment->potential-wires mapping 
; when a set of wires is known to be a single digit.
;
; Algorithm:
; * `W = ` the wires in the given pattern
; * `S = ` the segments that make up the given digit. 
; * For each segment `s`:
;   * if $s \in S$ then $s = s \bigcap W$ 
;   * otherwise $s = s - W$
;   
(defn update-segment->potential-wires-for-known-digit
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

; For example there is only one pattern with two wires. This pattern
; corresponds to the `1` digit. The segments `:C` and `:F` are updated like
; with set intersection:
;
; $C = C \bigcap \{a b\}$
;
; $F = F \bigcap \{a b\}$
; 
; all others are updated with
; 
; $X = X - \{a b\}$ 
; 
; This results in the `:C` and `:F` segments having the potential wires of only
; `:a` and  `:b`. The rest of the segments can't be `:a` or `:b` so those wires
; are removed from their potential wires.
(update-segment->potential-wires-for-known-digit init-segment->potential-wires #{:a :b} 1)


; This can be done with all the digits that are known right at the beginning:
; `1`, `7`, and `4`. `8` could also be done but it doesn't change the potential
; wires for any segment because all segments are used for 8.
(-> init-segment->potential-wires
    (update-segment->potential-wires-for-known-digit #{:a :b} 1)
    (update-segment->potential-wires-for-known-digit #{:d :a :b} 7)
    (update-segment->potential-wires-for-known-digit #{:e :b :f :a} 4))


; Now a new strategy is needed to reduce the possibilities. 
;
; The pattern `#{:c :b :d :f :a}` could still be a `2`, `3`, or `5` because
; those digits all use 4 segments. If any of the patterns with known digits
; are subsets of this pattern then it might be possible to remove some potential
; digits.
; 
; For example the pattern `#{:d :a :b}` is the pattern for `7` so it must use
; the segments `A`, `C`, and `F`. Because `#{:c :b :d :f :a}` is a superset of
; `#{:d :a :b}` then `#{:c :b :d :f :a}` must use the same segments.
; 
; Now since `#{:c :b :d :f :a}` can be a `2`, `3`, or `5` and must also use
; segments `A`, `C`, and `F` maybe some of the digits can be eliminated.
; 
; * `2` uses segments `#{:A :C :D :E :G}` which doesn't have `F` so `2` can be
; eliminated.
;
; * `3` uses segments `#{:A :C :D :F :G}` which is a superset of `#{:A :C :F}`
; so `3` is still a potential digit.
;
; * `5` uses segments `#{:A :B :D :F :G}` which is not a supser set of 
; `#{:A :C :F}` so `5` can be eliminated.
;
; After all this `#{:c :b :d :f :a}` must be the pattern for `3`. The mapping
; `pattern->possible-digits` can be updated. Also the `segment->potential`
; wires can be updated. 

(defn largest-subset-pattern-with-known-digit
  [pattern->possible-digits pattern]
  (->> pattern->possible-digits
       (filter #(and (= 1 (count (val %)))
                     (sets/subset? (key %) pattern)))
       (sort-by #(- (count (key %))))
       (first)))


(defn digit-contains-segments? 
  [digit segments]
  (sets/subset? segments (digit->segments digit)))


(defn potential-digits-by-subset
  [pattern->possible-digits pattern]
  (if-let [[_ ds] (largest-subset-pattern-with-known-digit pattern->possible-digits pattern)]
    (let [overlapping-segments (digit->segments (first ds))]
      (into #{}
            (filter #(digit-contains-segments? % overlapping-segments))
            (pattern->possible-digits pattern)))))


(potential-digits-by-subset
  (init-pattern->possible-digits test-patterns)
  #{:c :b :d :f :a})

(potential-digits-by-subset
  (init-pattern->possible-digits test-patterns)
  #{:e :c :b :d :f})

(potential-digits-by-subset
  (init-pattern->possible-digits test-patterns)
  #{:e :c :b :d :f})

(defn update-pattern->possible-digits-for-known-digit
  [pattern->possible-digits pattern digit]
  (reduce (fn [ppd [p pd]]
            (assoc ppd
                   p
                   (if (= p pattern)
                     #{digit}
                     (disj pd digit))))
          {}
          pattern->possible-digits))

(update-pattern->possible-digits-for-known-digit 
  (init-pattern->possible-digits test-patterns)
  #{:c :b :d :f :a}
  3)

(-> init-segment->potential-wires
    (update-segment->potential-wires-for-known-digit #{:a :b} 1)
    (update-segment->potential-wires-for-known-digit #{:d :a :b} 7)
    (update-segment->potential-wires-for-known-digit #{:e :b :f :a} 4)
    (update-segment->potential-wires-for-known-digit #{:c :b :d :f :a} 3))


; On paper I only did the 5 segment patterns. But it appears there might be 
; some more info
(let [pattern->possible-digits (init-pattern->possible-digits test-patterns)]
  (map (juxt identity #(potential-digits-by-subset pattern->possible-digits %))
       (keys pattern->possible-digits)))

; `#{:a :b :c :d :f} [3]`
; `#{:a :b :c :d :e :g} [0 9]`
; `#{:a :b :c :d :e :f} [9]`

(-> init-segment->potential-wires
    (update-segment->potential-wires-for-known-digit #{:a :b} 1)
    (update-segment->potential-wires-for-known-digit #{:d :a :b} 7)
    (update-segment->potential-wires-for-known-digit #{:e :b :f :a} 4)
    (update-segment->potential-wires-for-known-digit #{:c :b :d :f :a} 3)
    (update-segment->potential-wires-for-known-digit #{:a :b :c :d :e :f} 9)
    (update-segment->potential-wires-for-known-digit #{:a :b :c :d :e :g} 0))


; clerk isn't going to render this correctly!
(-> (init-pattern->possible-digits test-patterns)
    (update-pattern->possible-digits-for-known-digit #{:c :b :d :f :a} 3)
    (update-pattern->possible-digits-for-known-digit #{:a :b :c :d :e :f} 9)
    (update-pattern->possible-digits-for-known-digit #{:a :b :c :d :e :g} 0))



; Still need a third strategy for eliminating possibilities.
; Need to use known segment->potential-wires entries.

; Example:
;
; `#{:g :c :e :f :a}` can be one of `#{2 5}`
;
; Given this state of segment->potential-wires:
; `{:A #{:d} :B #{:e} :C #{:a :b} :D #{:f} :E #{:g} :F #{:a :b} :G #{:c}}`
; 
; Can the pattern be 2?
; Required segments `#{:A :C :D :E :G}` 
; 
; Map to known wires: `#{:d :f :g :c :a}` 
; 
;
(sets/subset? #{:d :f :g :c :a} #{:g :c :d :f :a})
; So it can be a 2.
;
; What about a 5?
; Required segments `#{:A :B :D :F :G}` 
;
; Map to known wires: `#{:d :e :f :c} 
(sets/subset? #{:d :e :f :c :a} #{:g :c :d :f :a})
; So it can't be a 5.
;
; 


; now there are valid mappings
(-> init-segment->potential-wires
    (update-segment->potential-wires-for-known-digit #{:a :b} 1)
    (update-segment->potential-wires-for-known-digit #{:d :a :b} 7)
    (update-segment->potential-wires-for-known-digit #{:e :b :f :a} 4)
    (update-segment->potential-wires-for-known-digit #{:c :b :d :f :a} 3)
    (update-segment->potential-wires-for-known-digit #{:a :b :c :d :e :f} 9)
    (update-segment->potential-wires-for-known-digit #{:a :b :c :d :e :g} 0)
    (update-segment->potential-wires-for-known-digit #{:g :c :d :f :a} 2))


; clerk isn't going to render this correctly!
(-> (init-pattern->possible-digits test-patterns)
    (update-pattern->possible-digits-for-known-digit #{:c :b :d :f :a} 3)
    (update-pattern->possible-digits-for-known-digit #{:a :b :c :d :e :f} 9)
    (update-pattern->possible-digits-for-known-digit #{:a :b :c :d :e :g} 0)
    (update-pattern->possible-digits-for-known-digit #{:g :c :d :f :a} 2))


(def example-pattern->digit
  (-> (init-pattern->possible-digits test-patterns)
      (update-pattern->possible-digits-for-known-digit #{:c :b :d :f :a} 3)
      (update-pattern->possible-digits-for-known-digit #{:a :b :c :d :e :f} 9)
      (update-pattern->possible-digits-for-known-digit #{:a :b :c :d :e :g} 0)
      (update-pattern->possible-digits-for-known-digit #{:g :c :d :f :a} 2)))



(def test-display
  (map #(into #{} (map (comp keyword str)) %) ["cdfeb" "fcadb" "cdfeb" "cdbaf"]))

; Well at least this is the correct answer!
(map (comp first example-pattern->digit) test-display)


; ## The question now is can I put this all together
;
; Algorithm
; * Input: a line from the input: the unique patterns and display patterns
; * Output: the number that the display shows 
; * Steps:
;    * initialize the `pattern->possible-digits` mapping
;    * initialize the `segment->potential-wires` mapping
;    * loop until the pattern->possible-digits is complete
; TODO add round creation here
;       * Strategy 2: the subset/overlapping thing
;       * Strategy 3: use segment->potential-wires strategy
;       * if a new known pattern->digit mapping is found then use Strategy 1
;         to update the mappings
;    * using the completed patter->potential-wires mapping map the display
;      patterns.

(defn init-segment->potential-wires2
  [pattern->possible-digits]
  (reduce (fn [segment->potential-wires [pattern possible-digits]]
            (if (= 1 (count possible-digits))
              (update-segment->potential-wires-for-known-digit
                segment->potential-wires
                pattern
                (first possible-digits))
              segment->potential-wires))
          init-segment->potential-wires 
          pattern->possible-digits))


(defn ambigous-patterns
  [pattern->possible-digits]
  (->> pattern->possible-digits
       (filter #(< 1 (count (val %))))
       (sort-by (comp count key))))


(defn done?
  [pattern->possible-digits]
  (every? #(= 1 (count (val %))) pattern->possible-digits))


(defn update-pattern->possible-digits
  [pattern->possible-digits pattern possible-digits]
  (reduce (fn [ppd [p pd]]
            (assoc ppd
                   p
                   (if (= p pattern)
                     (sets/intersection pd possible-digits)
                     (if (= 1 (count possible-digits))
                       (sets/difference pd possible-digits)
                       pd))))
          {}
          pattern->possible-digits))


(defn remove-known-digits
  [pattern->possible-digits]
  (let [known-digits (into #{}
                           (comp (map val)
                                 (filter #(= 1 (count %)))
                                 (map first))
                           pattern->possible-digits)]
    (reduce (fn [ppd [pattern digits]]
              (assoc ppd
                     pattern
                     (if (= 1 (count digits))
                       digits
                       (sets/difference digits known-digits))))
            {}
            pattern->possible-digits)))


(defn strategy2
  [pattern->possible-digits]
  (transduce (comp (map (juxt key
                              #(potential-digits-by-subset
                                 pattern->possible-digits (key %))))
                   (filter #(some? (second %))))
             (completing
               (fn [ppd [pattern digits]]
                 (update-pattern->possible-digits ppd pattern digits))
               remove-known-digits)
             pattern->possible-digits
             (ambigous-patterns pattern->possible-digits)))


(defn solve-single-display
  [{:keys [display unique]}]
  (let [pattern->possible-digits (init-pattern->possible-digits unique)
        segment->potential-wires (init-segment->potential-wires2 pattern->possible-digits)]
    (strategy2 pattern->possible-digits)))


(def test-line "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")

(solve-single-display (first (read-input test-line)))
(clojure.pprint/pprint (solve-single-display (first (read-input test-line))))

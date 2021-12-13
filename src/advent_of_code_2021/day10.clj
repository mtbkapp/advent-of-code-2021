;# [Day 10](https://adventofcode.com/2021/day/10)

(ns advent-of-code-2021.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]))


(def real-input (slurp (io/resource "day10.txt")))


(def test-input "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]")


(defn read-input
  [input]
  (map seq (string/split-lines input)))


(read-input test-input)


(def matching
  {\{ \}
   \[ \]
   \( \)
   \< \>})


; The key for this particular solution the stack data structure. In Clojure
; vectors can be stacks.
;
; The algorithm here looks at each character in order:
; * If the character is an open bracket of some kind it is pushed onto the stack
; * If the character is a closing character and matches the character on the
;   the top of the stack then that top character is popped from the stack.
; * If the closing character doesn't match the character on the top of the stack
;   then the line must be "corrupted". The character that shouldn't be there is
;   returned.
; * If the stack is empty and there are no more character then the line is valid.
; * If the there are no more characters and the stack is not empty then the
;   the line is "incomplete". The stack is returned in this case.
(defn check-syntax 
  ([cs] (check-syntax [] cs))
  ([stack [x & xs]]
   (cond (and (nil? x) (empty? stack)) {:result :valid}
         (nil? x) {:result :incomplete :stack stack}
         (contains? matching x) (recur (conj stack x) xs)
         (= x (get matching (peek stack))) (recur (pop stack) xs)
         :else {:result :corrupt :invalid-char x})))


(def points-for-char
  {\} 1197 
   \] 57
   \) 3 
   \> 25137})


(defn solve-part1
  [input]
  (transduce (comp (map check-syntax)
                   (filter #(= :corrupt (:result %)))
                   (map #(points-for-char (:invalid-char %))))
             +
             input))


(solve-part1 (read-input test-input))
(solve-part1 (read-input real-input))


(def points-for-char
  {\} 3 
   \] 2 
   \) 1 
   \> 4})


; The charcters that should be appended to an "incomplete" line have their 
; openning counterparts on the stack. Each character is popped from the stack,
; it's matching character is matched, and the score is updated.
(defn score-stack
  ([s] (score-stack s 0))
  ([s score]
   (if (empty? s)
     score
     (recur (pop s) (+ (* score 5) (points-for-char (matching (peek s))))))))


(defn get-middle
  [xs]
  (nth xs (/ (dec (count xs)) 2)))


(defn solve-part2
  [input]
  (->> input
       (map check-syntax)
       (filter #(= :incomplete (:result %)))
       (map #(score-stack (:stack %)))
       (sort)
       (get-middle)))


(solve-part2 (read-input test-input))
(solve-part2 (read-input real-input))


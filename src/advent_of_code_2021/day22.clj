;# [Day 15](https://adventofcode.com/2021/day/22)

(ns advent-of-code-2021.day22
  (:require [clojure.java.io :as io]
            [clojure.set :as sets]
            [clojure.string :as string]
            [clojure.test :refer :all]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]))


(def test-input
  "on x=10..12,y=10..12,z=10..12
on x=11..13,y=11..13,z=11..13
off x=9..11,y=9..11,z=9..11
on x=10..10,y=10..10,z=10..10")


(def test-input2
  "on x=-20..26,y=-36..17,z=-47..7
on x=-20..33,y=-21..23,z=-26..28
on x=-22..28,y=-29..23,z=-38..16
on x=-46..7,y=-6..46,z=-50..-1
on x=-49..1,y=-3..46,z=-24..28
on x=2..47,y=-22..22,z=-23..27
on x=-27..23,y=-28..26,z=-21..29
on x=-39..5,y=-6..47,z=-3..44
on x=-30..21,y=-8..43,z=-13..34
on x=-22..26,y=-27..20,z=-29..19
off x=-48..-32,y=26..41,z=-47..-37
on x=-12..35,y=6..50,z=-50..-2
off x=-48..-32,y=-32..-16,z=-15..-5
on x=-18..26,y=-33..15,z=-7..46
off x=-40..-22,y=-38..-28,z=23..41
on x=-16..35,y=-41..10,z=-47..6
off x=-32..-23,y=11..30,z=-14..3
on x=-49..-5,y=-3..45,z=-29..18
off x=18..30,y=-20..-8,z=-3..13
on x=-41..9,y=-7..43,z=-33..15
on x=-54112..-39298,y=-85059..-49293,z=-27449..7877
on x=967..23432,y=45373..81175,z=27513..53682")


(def test-input3
  "on x=-5..47,y=-31..22,z=-19..33
on x=-44..5,y=-27..21,z=-14..35
on x=-49..-1,y=-11..42,z=-10..38
on x=-20..34,y=-40..6,z=-44..1
off x=26..39,y=40..50,z=-2..11
on x=-41..5,y=-41..6,z=-36..8
off x=-43..-33,y=-45..-28,z=7..25
on x=-33..15,y=-32..19,z=-34..11
off x=35..47,y=-46..-34,z=-11..5
on x=-14..36,y=-6..44,z=-16..29
on x=-57795..-6158,y=29564..72030,z=20435..90618
on x=36731..105352,y=-21140..28532,z=16094..90401
on x=30999..107136,y=-53464..15513,z=8553..71215
on x=13528..83982,y=-99403..-27377,z=-24141..23996
on x=-72682..-12347,y=18159..111354,z=7391..80950
on x=-1060..80757,y=-65301..-20884,z=-103788..-16709
on x=-83015..-9461,y=-72160..-8347,z=-81239..-26856
on x=-52752..22273,y=-49450..9096,z=54442..119054
on x=-29982..40483,y=-108474..-28371,z=-24328..38471
on x=-4958..62750,y=40422..118853,z=-7672..65583
on x=55694..108686,y=-43367..46958,z=-26781..48729
on x=-98497..-18186,y=-63569..3412,z=1232..88485
on x=-726..56291,y=-62629..13224,z=18033..85226
on x=-110886..-34664,y=-81338..-8658,z=8914..63723
on x=-55829..24974,y=-16897..54165,z=-121762..-28058
on x=-65152..-11147,y=22489..91432,z=-58782..1780
on x=-120100..-32970,y=-46592..27473,z=-11695..61039
on x=-18631..37533,y=-124565..-50804,z=-35667..28308
on x=-57817..18248,y=49321..117703,z=5745..55881
on x=14781..98692,y=-1341..70827,z=15753..70151
on x=-34419..55919,y=-19626..40991,z=39015..114138
on x=-60785..11593,y=-56135..2999,z=-95368..-26915
on x=-32178..58085,y=17647..101866,z=-91405..-8878
on x=-53655..12091,y=50097..105568,z=-75335..-4862
on x=-111166..-40997,y=-71714..2688,z=5609..50954
on x=-16602..70118,y=-98693..-44401,z=5197..76897
on x=16383..101554,y=4615..83635,z=-44907..18747
off x=-95822..-15171,y=-19987..48940,z=10804..104439
on x=-89813..-14614,y=16069..88491,z=-3297..45228
on x=41075..99376,y=-20427..49978,z=-52012..13762
on x=-21330..50085,y=-17944..62733,z=-112280..-30197
on x=-16478..35915,y=36008..118594,z=-7885..47086
off x=-98156..-27851,y=-49952..43171,z=-99005..-8456
off x=2032..69770,y=-71013..4824,z=7471..94418
on x=43670..120875,y=-42068..12382,z=-24787..38892
off x=37514..111226,y=-45862..25743,z=-16714..54663
off x=25699..97951,y=-30668..59918,z=-15349..69697
off x=-44271..17935,y=-9516..60759,z=49131..112598
on x=-61695..-5813,y=40978..94975,z=8655..80240
off x=-101086..-9439,y=-7088..67543,z=33935..83858
off x=18020..114017,y=-48931..32606,z=21474..89843
off x=-77139..10506,y=-89994..-18797,z=-80..59318
off x=8476..79288,y=-75520..11602,z=-96624..-24783
on x=-47488..-1262,y=24338..100707,z=16292..72967
off x=-84341..13987,y=2429..92914,z=-90671..-1318
off x=-37810..49457,y=-71013..-7894,z=-105357..-13188
off x=-27365..46395,y=31009..98017,z=15428..76570
off x=-70369..-16548,y=22648..78696,z=-1892..86821
on x=-53470..21291,y=-120233..-33476,z=-44150..38147
off x=-93533..-4276,y=-16170..68771,z=-104985..-24507")


(def real-input (slurp (io/resource "day22.txt")))


(def range-pattern
  #"^(on|off)\sx=(\-?\d+)..(\-?\d+),y=(\-?\d+)..(\-?\d+),z=(\-?\d+)..(\-?\d+)$")


(defn parse-input
  [input]
  (map (fn [line]
         (let [[[_ op & limits]] (re-seq range-pattern (string/trim line))
               [xmin xmax ymin ymax zmin zmax] (map #(Long/valueOf %) limits)]
           [(keyword op) [[xmin xmax] [ymin ymax] [zmin zmax]]]))
       (string/split-lines input)))


(defn solve-part1
  [input]
  (transduce (comp (filter (fn [[_ [[x-min x-max] [y-min y-max] [z-min z-max]]]]
                             (and (<= -50 x-min x-max 50)
                                  (<= -50 y-min y-max 50)
                                  (<= -50 z-min z-max 50))))
                   (map (fn [[op [[x-min x-max] [y-min y-max] [z-min z-max]]]]
                          [op (for [x (range x-min (inc x-max))
                                    y (range y-min (inc y-max))
                                    z (range z-min (inc z-max))]
                                [x y z])])))
             (completing
               (fn [on-cubes [op cubes]]
                 (if (= :on op)
                   (sets/union on-cubes (set cubes))
                   (sets/difference on-cubes (set cubes))))
               count)
             #{}
             input))

#_(solve-part1 (parse-input test-input))
#_(solve-part1 (parse-input test-input2))
#_(solve-part1 (parse-input real-input))


; ## Part 2
; The trouble with the previous solution is that it's too slow. The remaining
; cuboids are much much larger.

; Let's see what happens if we combine the `x`, `y`, and `z` interval that 
; overlap.

(defn interval-comparator
  [[a-min a-max] [b-min b-max]]
  (if (= a-min b-min)
    (compare a-max b-max)
    (compare a-min b-min)))

(def empty-sorted-intervals
  (sorted-set-by interval-comparator))


; put each interval in the set
; then reduce from left to right as only adjacent intervals can be combined.

; can the combined intervals from the off steps be subtracted from the on steps?




(defn reduce-intervals
  [sorted-intervals]
  (reduce (fn [is [b-min b-max :as interval]]
            (let [[a-min a-max] (last is)]
              (if (<= a-min b-min a-max)
                (conj (subvec is 0 (dec (count is))) [a-min (max a-max b-max)])
                (conj is interval))))
          [(first sorted-intervals)]
          (rest sorted-intervals)))


(deftest test-reduce-intervals
  (is (= [[1 500] [501 1000]]
         (reduce-intervals [[1 3] [3 5] [4 500] [4 30] [501 505] [502 1000]]))))


(defn reduce-interval-sets
  [intervals]
  (-> intervals
      (update :xs reduce-intervals)
      (update :ys reduce-intervals)
      (update :zs reduce-intervals)))


(defn steps->interval-sets
  [steps]
  (-> (reduce (fn [is [op [xs ys zs]]]
                (-> is
                    (update-in [op :xs] conj xs)
                    (update-in [op :ys] conj ys)
                    (update-in [op :zs] conj zs)))
              {:off (zipmap [:xs :ys :zs] (repeat empty-sorted-intervals))
               :on (zipmap [:xs :ys :zs] (repeat empty-sorted-intervals))}
              steps)
      (update :off reduce-interval-sets)
      (update :on reduce-interval-sets)))

#_(steps->interval-sets (parse-input test-input3))
#_(steps->interval-sets (parse-input real-input))

; turns out that both sets of intervals make a cuboid each... 


; wo wo wo, is the off cuboid within the on cuboid? If so the answer is |on| - |off|
#_(steps->interval-sets (parse-input real-input))

(defn interval-within?
  [[[on-min on-max]] [[off-min off-max]]]
  (<= on-min off-min off-max on-max))

(defn off-within-on?
  [{:keys [off on]}]
  (and (interval-within? (:xs on) (:xs off))
       (interval-within? (:ys on) (:ys off))
       (interval-within? (:zs on) (:zs off))))

(off-within-on? (steps->interval-sets (parse-input real-input)))
; no

; fine


; How about this part2 = |on| - |off (intersect) on|?

(defn interval-sets->cuboids
  [{{[off-xs] :xs [off-ys] :ys [off-zs] :zs} :off
    {[on-xs] :xs [on-ys] :ys [on-zs] :zs} :on}]
  {:off [off-xs off-ys off-zs]
   :on [on-xs on-ys on-zs]})

(interval-sets->cuboids (steps->interval-sets (parse-input real-input)))

(defn intersect-interval
  [[a-min a-max] [b-min b-max]]
  (if-not (or (< a-max b-min) (< b-max a-min))
    (let [[_ i-min i-max _] (sort [a-min a-max b-min b-max])]
      [i-min i-max])))

(deftest test-intersect-interval
  (testing "non overlapping"
    (is (nil? (intersect-interval [1 10] [11 23])))
    (is (nil? (intersect-interval [11 23] [1 10]))))
  (testing "a within b"
    (is (= [5 7] (intersect-interval [5 7] [1 10])))
    (is (= [5 7] (intersect-interval [5 7] [5 10])))
    (is (= [5 10] (intersect-interval [5 10] [1 10]))))
  (testing "b within a"
    (is (= [5 7] (intersect-interval [1 10] [5 7])))
    (is (= [5 7] (intersect-interval [5 10] [5 7])))
    (is (= [5 10] (intersect-interval [1 10] [5 10]))))
  (testing "overlap a then b"
    (is (= [4 5] (intersect-interval [1 5] [4 7])))
    (is (= [5 5] (intersect-interval [1 5] [5 7]))))
  (testing "overlap b then a"
    (is (= [4 5] (intersect-interval [4 7] [1 5])))
    (is (= [5 5] (intersect-interval [5 7] [1 5]))) )
  (testing "same interval"
    (is (= [10 10] (intersect-interval [10 10] [10 10])))))


(defn intersect-cuboids
  [[a-xs a-ys a-zs] [b-xs b-ys b-zs]]
  [(intersect-interval a-xs b-xs)
   (intersect-interval a-ys b-ys)
   (intersect-interval a-zs b-zs)])


(deftest test-intersect-cuboids
  (is (= [[3 3] [3 3] [2 2]]
         (intersect-cuboids [[1 3] [1 3] [1 2]]
                            [[3 8] [3 4] [2 3]]))))


(defn cuboid-size
  [[[x-min x-max] [y-min y-max] [z-min z-max]]]
  (* (- (inc x-max) x-min)
     (- (inc y-max) y-min)
     (- (inc z-max) z-min)))


(deftest test-cuboid-size
  (is (= 1 (cuboid-size [[1 1] [2 2] [3 3]])))
  (is (= 1000 (cuboid-size [[1 10] [1 10] [1 10]])))
  (is (= (* 21 21 21) (cuboid-size [[-10 10] [-10 10] [-10 10]])))
  (is (= 1000 (cuboid-size [[-10 -1] [-10 -1] [-10 -1]])))
  (is (= 1 (cuboid-size [[3 3] [3 3] [2 2]]))))


#_(= 2758514936282235 5311270970242160)

#_(prn (let [{:keys [on off]} (interval-sets->cuboids (steps->interval-sets (parse-input test-input3)))]
         (- (cuboid-size on)
            (cuboid-size (intersect-cuboids off on)))
         ))

#_(prn (let [{:keys [on off]} (interval-sets->cuboids (steps->interval-sets (parse-input real-input)))]
       (- (cuboid-size on)
          (cuboid-size (intersect-cuboids on off)))
       ))


; 765696665928344 fail


; I think this doesn't work because of order of operations is ignored. 
; In the above calculation a cube is assumed to be off if it ever is in an off
; cuboid. However if the it appeared in an off cuboid an then in an on cuboid
; it should be considered to be on.

; ok fine so, try 5;


(defn combine-intervals
  [is]
  (reduce (fn [ss [b-min b-max :as curr]]
            (let [[a-min a-max :as prev] (last ss)]
              (if (<= a-min b-min a-max)
                (conj (disj ss prev) [a-min (max a-max b-max)])
                (conj ss curr))))
          (conj empty-sorted-intervals (first is))
          (rest is)))



(defn add-interval
  [is interval]
  (combine-intervals (conj is interval)))

(deftest test-add-interval
  (is (= (sorted-set [2 3])
         (add-interval empty-sorted-intervals [2 3])))
  (is (= (sorted-set [2 3] [5 10])
         (-> empty-sorted-intervals
             (add-interval [2 3])
             (add-interval [5 10]))))
  (is (= (sorted-set [2 3] [4 10])
         (-> empty-sorted-intervals
             (add-interval [2 3])
             (add-interval [5 10])
             (add-interval [4 7]))))
  (is (= (sorted-set [-10 0] [2 3] [4 10])
         (-> empty-sorted-intervals
             (add-interval [2 3])
             (add-interval [5 10])
             (add-interval [4 7])
             (add-interval [-10 0]))))
  (is (= (sorted-set [-10 10])
         (-> empty-sorted-intervals
             (add-interval [2 3])
             (add-interval [5 10])
             (add-interval [4 7])
             (add-interval [-10 0])
             (add-interval [-10 10])))))


(defn clip
  "Remove b from a. If nothing is remaining returns nil otherwise the clipped
  interval"
  [[a-min a-max :as a] [b-min b-max]]
  (cond
    (< a-max b-min) [a] ; non overlapping a then b
    (< b-max a-min) [a] ; non overlapping b then a
    (<= b-min a-min a-max b-max) [] ; b completely covers a
    (< a-min b-min b-max a-max) [[a-min (dec b-min)] [(inc b-max) a-max]] ; b within a, bounds not equal
    (and (= a-min b-min) (< b-max a-max)) [[(inc b-max) a-max]] ; a and b start at same place
    (and (= a-max b-max) (< a-min b-min)) [[a-min (dec b-min)]] ; a and b end at same place
    (and (< a-min b-min) (<= b-min a-max)) [[a-min (dec b-min)]] ; a then b with intersection
    (and (< b-min a-min) (<= a-min b-max)) [[(inc b-max) a-max]] ; b then a with intersection
    ))


(deftest test-clip
  (testing "non overlapping a then b"
    (is (= [[1 3]] (clip [1 3] [4 8]))))
  (testing "non overlapping b then a"
    (is (= [[1 3]] (clip [1 3] [-8 0]))))
  (testing "b covers a"
    (is (= [] (clip [1 5] [1 5])))
    (is (= [] (clip [1 5] [1 6])))
    (is (= [] (clip [1 5] [0 5])))
    (is (= [] (clip [1 5] [0 6]))))
  (testing "b inside a"
    (is (= [[1 1] [5 5]] (clip [1 5] [2 4])))
    (is (= [[-50 1] [5 50]] (clip [-50 50] [2 4]))))
  (testing "a-min = b-min"
    (is (= [[5 5]] (clip [1 5] [1 4])))
    (is (= [[5 8]] (clip [1 8] [1 4]))))
  (testing "a-max = b-max"
    (is (= [[1 1]] (clip [1 5] [2 5])))
    (is (= [[1 3]] (clip [1 5] [4 5]))))
  (testing "overlap with overhang a then b"
    (is (= [[1 1]] (clip [1 5] [2 6])))
    (is (= [[1 4]] (clip [1 5] [5 6])))
    (is (= [[1 3]] (clip [1 5] [4 7]))))
  (testing "overlap with overhang b then a"
    (is (= [[2 5]] (clip [1 5] [0 1])))
    (is (= [[4 5]] (clip [1 5] [0 3])))))


(defn del-interval
  [is interval]
  (into empty-sorted-intervals
        (mapcat #(clip % interval))
        is))


(deftest test-del-interval
  (is (= (into empty-sorted-intervals [[0 2] [9 16] [21 22]])
         (-> empty-sorted-intervals
             (add-interval [0 11])
             (add-interval [8 15])
             (add-interval [19 22])
             (del-interval [3 7])
             (del-interval [14 20])
             (del-interval [14 20])
             (add-interval [11 16])
             (del-interval [6 12])
             (add-interval [9 13])))))


#_(reduce (fn [s [op [xs ys zs]]]
          (let [op-f (if (= :on op) add-interval del-interval)]
            (-> s
                (update :xs op-f xs)
                (update :ys op-f ys)
                (update :zs op-f zs))))
        {:xs empty-sorted-intervals
         :ys empty-sorted-intervals
         :zs empty-sorted-intervals}
        (parse-input test-input3))


(defn interval-size
  [[a-min a-max]]
  (- (inc a-max) a-min))

(defn count-cubes
  [is]
  (reduce (fn [c [_ intervals]]
            (* c (reduce + (map interval-size intervals))))
          1
          is))

; 492340769869710
; 2758514936282235

#_(println ">>>>"
  (->
    (reduce (fn [s [op [xs ys zs]]]
              (let [op-f (if (= :on op) add-interval del-interval)]
                (-> s
                    (update :xs op-f xs)
                    (update :ys op-f ys)
                    (update :zs op-f zs))))
            {:xs empty-sorted-intervals
             :ys empty-sorted-intervals
             :zs empty-sorted-intervals}
            (parse-input real-input))
    count-cubes))

; 727708580029575 too low!


; for clerk, run tests here!
; main algorithm
;
; state = set of on cuboids
; step = op = #{on off}, step-cuboid
;
; op = off
; for each curr cuboid that intersects with the step cuboid split it into
; smaller cuboids excluding the intersection
;
; op = on
; do step for op = off
; then add cuboid
;
; finalize
; sum the sizes of the curr cuboids


; cuboid - cuboid = smaller cuboids (or no cuboids)
; Since I think the main algorithm will do lots of pairwise intersections
; the number of smaller cuboids should be minimum. So I'm going to consider
; all possible intersections. There are 10.
; TODO add math for calculating 10 intersections


(def split-cuboid nil)
(defmulti split-cuboid (fn [A B A-clipped]
                  (into #{}
                        (map count)
                        (vals A-clipped))))

(def clip 1)
(def all 0)
(def split 2)


; C C C | remove corner from B
(defmethod split-cuboid #{clip} 
  [A B A-clipped]
  )


; A A A | A inside B 
(defmethod split-cuboid #{all}
  [A B inter]
  [])


; S S S | B inside A
(defmethod split-cuboid #{split}
  [A B inter]
  
  )

; C C A | B removes cuboid edge from A 
(defmethod split-cuboid #{clip all}
  [A B inter]
  )


; C C S | partial bisect 
(defmethod split-cuboid #{clip all}
  [A B inter]
  )


; A A C
(defmethod split-cuboid #{all clip}
  [A B inter]
  )


; A A S | B bisects A
(defmethod split-cuboid #{all split}
  [A B A_clipped]
  (reduce (fn [sub-cuboids [k interval]]
            (if (= 2 (count interval))
              (-> sub-cuboids
                  (assoc-in [0 k (nth interval 0)])
                  (assoc-in [1 k (nth interval 1)]))
              (-> sub-cuboids
                  (assoc-in [0 k] (get A k))
                  (assoc-in [1 k] (get A k)))))
          [{} {}]
          A_clipped)
  )


; S S A
(defmethod split-cuboid #{split all}
  [A B {}]
  )


; S S C
(defmethod split-cuboid #{split clip}
  [A B inter]
  )


; C A S | B partial bisects A
(defmethod split-cuboid #{clip all split}
  [A B A-clipped]
  (reduce (fn [sub-cuboids [k interval]]
            (case (count interval)
              0 (let [ai (get A k)] 
                  (-> sub-cuboids
                      (assoc-in [0 k] ai)
                      (assoc-in [1 k] ai)
                      (assoc-in [2 k] ai)))
              1 (-> sub-cuboids
                    (assoc-in [0 k] (get A k))           ; left
                    (assoc-in [0 k] (nth interval 0))    ; middle
                    (assoc-in [0 k] (get A k)))          ; right
              2 (-> sub-cuboids
                    (assoc-in [0 k] (nth interval 0) )   ; left
                    (assoc-in [1 k] (get B k))           ; middle
                    (assoc-in [2 k] (nth interval 1))))) ; right
          [{} {} {}] 
          A-clipped))




; another idea
;
; state = map of cuboids to sub-cuboids that should be subtracted from the original
; problem what happens when the sub-cuboids overlap?


; fail fail fail
; negative volume is the answer!!!!!!

; https://todd.ginsberg.com/post/advent-of-code/2021/day22/

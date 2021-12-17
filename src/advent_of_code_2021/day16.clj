;# [Day 16](https://adventofcode.com/2021/day/16)

(ns advent-of-code-2021.day16
  (:require [clojure.java.io :as io]
            [clojure.set :as sets]
            [clojure.string :as string]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v])
  (:import [java.io StringReader]))


(def real-input (slurp (io/resource "day16.txt")))


(def hex-digit->bin-str
  {\0 "0000"
   \1 "0001"
   \2 "0010"
   \3 "0011"
   \4 "0100"
   \5 "0101"
   \6 "0110"
   \7 "0111"
   \8 "1000"
   \9 "1001"
   \A "1010"
   \B "1011"
   \C "1100"
   \D "1101"
   \E "1110"
   \F "1111"})


(defn read-input
  [input]
  (apply str (map #(hex-digit->bin-str %) input)))


(defn bin->dec
  [bin]
  (Long/valueOf bin 2))


(defn read-n-bits
  [rdr n]
  (reduce (fn [s _]
            (str s (char (.read rdr))))
          ""
          (range n)))


(defn read-version
  [rdr]
  (bin->dec (read-n-bits rdr 3)))


(def literal-packet-type-id 4)


(defn read-type-id
  [rdr]
  (bin->dec (read-n-bits rdr 3)))


(defn read-literal
  [rdr]
  (loop [word (read-n-bits rdr 5)
         bits ""]
    (let [next-bits (apply str bits (rest word))]
      (if (= \0 (first word))
        (bin->dec next-bits)
        (recur (read-n-bits rdr 5) next-bits)))))


(defn read-op-len
  [rdr]
  (let [len-type (read-n-bits rdr 1)
        len (read-n-bits rdr (if (= len-type "0") 15 11))]
    {:len-type (if (= len-type "0") :bits :packets)
     :len (bin->dec len)}))


(declare parse-packet)

(declare parse-packet*)


(defn has-more?
  [rdr]
  (.mark rdr 1)
  (let [c (.read rdr)]
    (if (= -1 c)
      false
      (do (.reset rdr)
          true))))


(defn read-subpackets-bit-count
  [rdr len]
  (loop [rdr (StringReader. (read-n-bits rdr len))
         packets []]
    (if (has-more? rdr)
      (recur rdr (conj packets (parse-packet* rdr)))
      packets)))


(defn read-subpackets-pack-count
  [rdr pack-count]
  (mapv (fn [_]
          (parse-packet* rdr))
        (range pack-count)))


(defn read-operator
  [rdr]
  (let [{:keys [len-type len]} (read-op-len rdr)]
    (case len-type
      :bits (read-subpackets-bit-count rdr len)
      :packets (read-subpackets-pack-count rdr len))))


(defn read-body
  [tid rdr]
  (if (= literal-packet-type-id tid)
    (read-literal rdr)
    (read-operator rdr)))


(defn parse-packet*
  [rdr]
  (let [v (read-version rdr)
        tid (read-type-id rdr)
        body (read-body tid rdr)]
    {:version v
     :packet-type tid
     :body body}))


(defn parse-packet
  [bits]
  (parse-packet* (StringReader. bits)))


(defn sum-version-nums
  [{:keys [version packet-type body]}]
  (if (= literal-packet-type-id packet-type)
    version
    (reduce (fn [sum packet]
              (+ sum (sum-version-nums packet)))
            version
            body)))


(defn solve-part1
  [hex]
  (-> (read-input hex)
      (parse-packet)
      (sum-version-nums)))


(solve-part1 real-input)


(def packet-types
  {0 :sum
   1 :product
   2 :min
   3 :max
   literal-packet-type-id :literal
   5 :>
   6 :<
   7 :=})


(def eval-packet nil)
(defmulti eval-packet
  (fn [{:keys [packet-type] :as packet}]
    (get packet-types packet-type)))


(defmethod eval-packet :literal
  [{:keys [body]}]
  body)


(defn eval-reduce-body
  [f init body]
  (reduce (fn [acc packet]
            (f acc (eval-packet packet)))
          init
          body))


(defmethod eval-packet :sum
  [{:keys [body]}]
  (eval-reduce-body + 0 body))


(defmethod eval-packet :product
  [{:keys [body]}]
  (eval-reduce-body * 1 body))


(defmethod eval-packet :min
  [{:keys [body]}]
  (eval-reduce-body min Long/MAX_VALUE body))


(defmethod eval-packet :max
  [{:keys [body]}]
  (eval-reduce-body max Long/MIN_VALUE body))


(defn eval-predicate
  [f {[x y] :body}]
  (if (f (eval-packet x) (eval-packet y)) 1 0))


(defmethod eval-packet :>
  [body]
  (eval-predicate > body))


(defmethod eval-packet :<
  [body]
  (eval-predicate < body))


(defmethod eval-packet :=
  [body]
  (eval-predicate = body))


(defn solve-part2
  [input]
  (-> (read-input input)
      (parse-packet)
      (eval-packet)))


(solve-part2 real-input)

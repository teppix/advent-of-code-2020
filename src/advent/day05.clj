(ns advent.day05
  (:require [advent.util :refer [get-resource]]
            [clojure.string :as str]))

(def real-data (str/split-lines (get-resource "real")))

(def shift #(bit-or %1 (bit-shift-left %2 1)))
(def dn (partial shift 0))
(def up (partial shift 1))

(defn step [state op]
  (apply update state
        (case op \B [:y up], \F [:y dn], \L [:x dn], \R [:x up])))

(defn get-seat-id [{:keys [x y]}] (+ x (* y 8)))

(defn decode-seat [code]
  (get-seat-id (reduce step {:x 0 :y 0} code)))

(assert (= 567 (decode-seat "BFFFBBFRRR")))
(assert (= 119 (decode-seat "FFFBBBFRRR")))
(assert (= 820 (decode-seat "BBFFBBFRLL")))

(defn is-gap? [[a b]]
  (and (some? a)
       (some? b)
       (= 2 (- b a))))

(assert (is-gap? [1 3]))
(assert (is-gap? [5 7]))
(assert (not (is-gap? [3 8])))
(assert (not (is-gap? [3 nil])))

(defn find-empty-seat [input]
  (if (is-gap? input)
    (+ 1 (first input))
    (recur (rest input))
    ))

(assert (= 4 (find-empty-seat [1 2 3 5 6])))
(assert (= 13 (find-empty-seat [10 11 12 14 15])))

(comment
  ; Answer 1
  (apply max (map decode-seat real-data))
  ; Answer 2
  (find-empty-seat (sort (map decode-seat real-data)))
  )

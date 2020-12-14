(ns advent.day12
  (:require [advent.util :refer [get-resource]]
            [clojure.string :as str]))

(def test-data (str/split-lines (get-resource "test")))
(def real-data (str/split-lines (get-resource "real")))

(defn parse-cmd [s]
  (let [[_ op arg] (re-find #"^(\w)(\d+)$" s)]
    [(keyword op) (Long/parseLong arg)]))

(defn vadd [[a1 a2] [b1 b2]] [(+ a1 b1) (+ a2 b2)])
(defn vmul [x [a b]] [(* x a) (* x b)])
(defn cw [[a b]] [b (- a)])
(defn ccw [[a b]] [(- b) a])

(defn n-times [n f]
  (apply comp (repeat n f)))

(defn rot [f vec n]
    ((n-times (quot n 90) f) vec))

(def left (partial rot ccw))
(def right (partial rot cw))

(defn result [s]
  (let [[a b] (:b s)]
    (+ (Math/abs a) (Math/abs b))))

(def initial-state-v1
  {:d [1 0]
   :b [0 0]})

(defn step-v1 [st cmd]
  (let [[op arg] (parse-cmd cmd)
        |> (partial assoc st)]
    (case op
      :N (|> :b (vadd (:b st) (vmul arg [0 1]))) ;; OK
      :S (|> :b (vadd (:b st) (vmul arg [0 -1])))
      :E (|> :b (vadd (:b st) (vmul arg [1 0])))
      :W (|> :b (vadd (:b st) (vmul arg [-1 0])))
      :R (|> :d (right (:d st) arg))
      :L (|> :d (left (:d st) arg))
      :F (|> :b (vadd (:b st) (vmul arg (:d st)))) ;; OK
      )))

(defn answer-1 [cmds]
  (->> cmds
    (reduce step-v1 initial-state-v1)
    (result)
    ))

(assert (= 25 (answer-1 test-data)))

(comment 
  ;; Answer 1
  (answer-1 real-data))


;; Part 2

(def initial-state-v2
  {:w [10 1]
   :b [0 0]})

(defn step-v2 [st cmd]
  (let [[op arg] (parse-cmd cmd)
       |> (partial assoc st)]
    (case op
      :N (|> :w (vadd (:w st) (vmul arg [0 1])))
      :S (|> :w (vadd (:w st) (vmul arg [0 -1])))
      :E (|> :w (vadd (:w st) (vmul arg [1 0])))
      :W (|> :w (vadd (:w st) (vmul arg [-1 0])))
      :R (|> :w (right (:w st) arg))
      :L (|> :w (left (:w st) arg))
      :F (|> :b (vadd (:b st) (vmul arg (:w st))))
      )))

(defn answer-2 [cmds]
  (->> cmds
       (reduce step-v2 initial-state-v2)
       result
       ))

(assert (= 286 (answer-2 test-data)))

(comment
  ;; Answer 2
  (answer-2 real-data))

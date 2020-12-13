(ns advent.day11
  (:require [advent.util :refer [get-resource]]
            [clojure.string :as str]
            [clojure.set :as set]))

(def test-data (get-resource "test"))
(def real-data (get-resource "real"))

(def verbose false)
(comment
  (def verbose true))

;; Common

(defn to-xy [i len] [(mod i len) (quot i len)])

(defn parse-seatmap [data]
  (let [len (str/index-of data "\n")]
    (into {}
          (for [[i ch] (map-indexed vector data)
                :when (and (not= ch \newline)
                           (not= ch \.))]
            [(to-xy i (inc len)) ({\L false \# true} ch)]))))


(defn draw [seatmap text w h]
  (when verbose
    (println text)
    (println
     (str/join
      (for [y (range h)
            x (range (inc w))]
        (if (= x w)
          \newline
          (case (seatmap [x y])
            true \#
            false \L
            nil \.))))))
  seatmap)

(defn converge [state f]
  (let [state' (f state)]
    (if (= state state')
      state
      (recur state' f)
      )))

(defn count-seats [seatmap]
  (count (filter identity (vals seatmap))))

(defn seatmap-dimensions [input]
  (let [lines (str/split-lines input)
        w (count (first lines))
        h (count lines)
        ]
    [w h]))

;; Part 1

(defn adjacent [x y]
  (for [dx (range -1 2)
        dy (range -1 2)
        :when (not= 0 dx dy)]
    [(+ x dx) (+ y dy)])
  )

(defn count-adj [seatmap [x y]]
  (count (filter identity (map #(seatmap %) (adjacent x y)))))

(defn step-v1-cond [occ adj]
  (cond
    (and (not occ) (= adj 0)) true
    (and occ (<= 4 adj)) false
    :else occ))

(defn step-v1 [seatmap]
  (let [next-seat #(vector % (step-v1-cond (seatmap %) (count-adj seatmap %)))
        ]
    (into {}
          (map next-seat (keys seatmap)))))

(defn answer-1 [input]
  (let [initial (parse-seatmap input)
        [w h] (seatmap-dimensions input)]
    (-> initial
        (draw "initial" w h)
        (converge step-v1)
        (draw "final" w h)
        (count-seats)
        )))

(assert (= 37 (answer-1 test-data)))

(comment
  ; Answer 1
  (answer-1 real-data))

;; Part 2

(defn addv [[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)])

(defn trace [seatmap max pos dir]
  (let [pos' (addv pos dir)
        occ (seatmap pos')
        ]
    (if (some? occ)
      occ
      (if (= 0 max)
        false
        (recur seatmap (dec max) pos' dir)))))

(def directions [[0 1] [1 1] [1 0] [1 -1] [0 -1] [-1 -1] [-1 0] [-1 1]])

(defn count-vis [seatmap max pos]
  (let [tr (partial trace seatmap max pos)]
    (as-> directions v
      (count (filter tr v))
      ))
  )

(defn step-v2-cond [occ vis]
  (cond
    (and (not occ) (= vis 0)) true
    (and occ (<= 5 vis)) false
    :else occ))

(defn step-v2 [max seatmap]
  (let [next-seat #(vector % (step-v2-cond (seatmap %) (count-vis seatmap max %)))
        ]
    (into {} (map next-seat (keys seatmap)))))

(defn answer-2 [input]
  (let [initial (parse-seatmap input)
        [w h] (seatmap-dimensions input)
        tmax (max w h)
        step (partial step-v2 tmax)]
    (-> initial
        (draw "initial" w h)
        (converge step)
        (draw "final" w h)
        (count-seats)
        )))


(assert (= 26 (answer-2 test-data)))

(comment
  ;; Answer 2
  (time (answer-2 real-data)))

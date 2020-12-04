(ns advent.day03
  (:require [advent.util :refer [get-resource]]
            [clojure.string :as str])
  )

(def test-data (str/split-lines (get-resource "test")))
(def real-data (str/split-lines (get-resource "real")))

(defn get-tile [data x y]
  (try (let [line (nth data (dec y))
             char (nth line (mod (dec x) (count line)))
             ]
         char)
       (catch Exception e nil))
  )

(defn walk [data x-step y-step]
  (rest ;; skip start position
    (loop [x 1, y 1, acc []]
      (let [tile (get-tile data x y)]
        (if (nil? tile)
          ;; end of the forest
          acc
          ;; keep walking
          (recur
            (+ x x-step) (+ y y-step)
            (conj acc tile))
          )))))

(defn count-trees [path]
  ((frequencies path) \#))

(assert (= 7 (count-trees (walk test-data 3 1))))
(assert (= 284 (count-trees (walk real-data 3 1))))

(comment
  ; Answer 1
  (count-trees (walk real-data 3 1))
  ; Answer 2
  (apply * (map
             (fn [[x y]] (count-trees (walk real-data x y)))
             [[1 1] [3 1] [5 1] [7 1] [1 2]])))

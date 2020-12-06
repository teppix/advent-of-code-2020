(ns advent.day06
  (:require [advent.util :refer [get-resource split-groups]]
            [clojure.set :refer [union intersection]]))

(def test-data (split-groups (get-resource "test")))
(def real-data (split-groups (get-resource "real")))

(defn merge-group [f group] (apply f (map set group)))
(defn total [f data] (apply + (map (comp count (partial merge-group f)) data)))

(assert (= 11 (total union test-data)))
(assert (= 6551 (total union real-data)))
(assert (= 6 (total intersection test-data)))

(comment
  ; Answer 1
  (total union real-data)
  ; Answer 2
  (total intersection real-data))

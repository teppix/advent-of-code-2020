(ns advent.day07
  (:require [advent.util :refer [get-resource split-groups]]
            [clojure.set :refer [union intersection]]
            [clojure.string :as string]))

(def test1-data (string/split-lines (get-resource "test1")))
(def test2-data (string/split-lines (get-resource "test2")))
(def real-data (string/split-lines (get-resource "real")))

;; Part 1

(defn parse-line-v1 [line]
  (let [[mpar & mch] (re-seq #"(\w+ \w+) bag" line)]
    [(second mpar) (map second mch)]))

(defn ancestors [tbl c]
  (let [ps (tbl c)]
    (if (empty? ps)
      []
      (distinct (apply concat ps (map (partial ancestors tbl) ps))))))

(defn answer1 [data color]
  (let [flip (fn [[p c]] (mapv #(vec [% p]) c))
        tbl (->> data
                 (mapv parse-line-v1)
                 (mapv flip)
                 (apply concat)
                 (reduce (fn [acc [k v]] (update acc k conj v)) {}))
        ]
    (count (ancestors tbl color))
    ))

(assert (= 4 (answer1 test1-data "shiny gold")))

(comment
  ; Answer 1
  (answer1 real-data "shiny gold"))

;; Part 2

(defn parse-line-v2 [line]
  (let [mpar (re-find #"(\w+ \w+) bags contain" line)
        mch (re-seq #"(\d+) (\w+ \w+) bag" line)]
    [(second mpar) (map #(vec [(Integer/parseInt (second %)) (nth % 2)]) mch)]
    ))

(defn count-children [tbl [n c]]
  (let [cs (tbl c)]
    (if (empty? cs)
      n
      (+ n (* n (apply + (map (partial count-children tbl) cs))))
      )))

(defn answer2 [data color]
  (let [tbl (->> data
                 (mapv parse-line-v2)
                 (apply concat)
                 (apply hash-map))]
    (dec (count-children tbl [1 color]))))

(assert (= 32 (answer2 test1-data "shiny gold")))
(assert (= 126 (answer2 test2-data "shiny gold")))

(comment
  ; Answer 2
  (answer2 real-data "shiny gold"))

(ns advent.day02
  (:require [advent.util :refer [get-resource]])
  )

(def test-data "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc")
(def real-data (get-resource "real"))

(defn parse-rule [rule]
  (let [[from to char password] (rest (re-find #"^([\d]+)-([\d]+) (.): (.*)$" rule))]
    [(Integer/parseInt from) (Integer/parseInt to) (first char) password]))

(defn password-valid-v1? [[from to char password]]
  (let [freq ((frequencies password) char 0)]
    (<= from freq to)))

(defn password-valid-v2? [[index-1 index-2 char password]]
  (let [char-at #(= char (nth password (- % 1)))]
    (not= (char-at index-1) (char-at index-2))))

(defn count-valid [is-valid? in-data]
  (count (filter is-valid? (map parse-rule (clojure.string/split-lines in-data)))))

(assert (== 2 (count-valid password-valid-v1? test-data)))
(assert (== 1 (count-valid password-valid-v2? test-data)))

(assert (== 500 (count-valid password-valid-v1? real-data)))
(assert (== 313 (count-valid password-valid-v2? real-data)))

(comment
  ;; Answer 1
  (count-valid password-valid-v1? real-data)
  ;; Answer 2
  (count-valid password-valid-v2? real-data))

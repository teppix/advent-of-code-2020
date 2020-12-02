(ns advent.day02)

(def test-data "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc")
(def real-data (slurp "./resources/day02.txt"))

(defn xor [a b]
  (or (and a (not b))
      (and b (not a))))

(defn parse-rule [rule]
  (let [[from to char password] (rest (re-find #"^([\d]+)-([\d]+) (.): (.*)$" rule))]
    [(Integer/parseInt from) (Integer/parseInt to) (first char) password]))

(defn password-valid-v1? [[from to char password]]
  (let [freq ((frequencies password) char 0)]
    (<= from freq to)))

(defn password-valid-v2? [[index-1 index-2 char password]]
  (let [ check #(= char (nth password (- % 1))) ]
    (xor (check index-1) (check index-2))))

(defn count-valid [is-valid? in-data]
  (count (filter is-valid? (map parse-rule (clojure.string/split-lines in-data)))))

(assert (== 2 (count-valid password-valid-v1? test-data)))
(assert (== 1 (count-valid password-valid-v2? test-data)))

(comment
  ;; Answer 1
  (count-valid password-valid-v1? real-data)
  ;; Answer 2
  (count-valid password-valid-v2? real-data))

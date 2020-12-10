(ns advent.day09
  (:require [advent.util :refer [get-resource]]
            [clojure.math.combinatorics :refer [combinations]]))

(defn load-data [s] (->> s
                         get-resource
                         (clojure.string/split-lines)
                         (map #(Long/parseLong %))))

(def test-data (load-data "test"))
(def real-data (load-data "real"))

;; Part 1

(defn sum [arg] (apply + arg))

(defn pair-sums [ns]
  (->> (combinations ns 2)
       (reduce #(conj %1 (sum %2)) #{})))

(defn find-invalid [len input]
  (let [ns (take len input)
        n (nth input len)]
    (if (contains? (pair-sums ns) n)
      (recur len (drop 1 input))
      n)))

(assert (= 127 (find-invalid 5 test-data)))

(comment
  ; Answer 1
  (find-invalid 25 real-data))

;; Part 2

(defn seq-sums
  ([ns] (seq-sums ns ns (first ns) (first ns)))
  ([[a b & rest] [n & ns] n-min n-max]
   (let [n-min (min n-min n)
         n-max (max n-max n)]
     (if (nil? b)
       (cons [a n-min n-max] (repeat nil))
       (lazy-seq (cons [a n-min n-max] (seq-sums (cons (+ a b) rest) ns n-min n-max)))))))

(take 3 test-data)
(take 5 (seq-sums test-data))

(defn test-range [a [sum & sums]]
  (let [[tot n-min n-max] sum]
    (cond
      (nil? sum) nil
      (< a tot) nil
      (== a tot) (+ n-min n-max)
      :else (recur a sums))))

(defn find-range [n len input]
  (let [sums (seq-sums input)]
    (or (test-range n sums)
        (recur n len (rest input)))))


(assert (= 62 (find-range 127 5 test-data)))

(comment
  ; Answer 2
  (find-range 1639024365 25 real-data)
  )
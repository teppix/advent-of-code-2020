(ns advent.day10
  (:require [advent.util :refer [get-resource]]))

(defn load-data [s] (->> s
                         get-resource
                         (clojure.string/split-lines)
                         (map #(Long/parseLong %))))

(def test-data-1 [16 10 15 5 1 11 7 19 6 12 4 ])
(def test-data-2 [ 28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32 25 35 8 17 7 9 4 2 34 10 3 ])
(def real-data (load-data "real"))

;; Part 1

(defn diffs [[ fst snd & rst ]]
  (if (nil? snd)
        (cons 3 nil)
        (cons (- snd fst) (diffs (cons snd rst)))))

(defn mul [freq]
  (prn freq)
  (* (freq 1 0) (freq 3 0)))

(defn ans-1 [data]
  (-> (cons 0 data)
      sort
      diffs
      frequencies
      mul
      ))

(assert (= 35 (ans-1 test-data-1)))
(assert (= 220 (ans-1 test-data-2)))

(comment
  ; Answer 1
  (ans-1 real-data))

;; Part 2

(def rev-sort (partial sort (comp - compare)))
(defn step [coll n]
  (let [conns (apply + [(get coll (+ n 1) 0)
                        (get coll (+ n 2) 0)
                        (get coll (+ n 3) 0)])]
        (assoc coll n conns)))

(defn ans-2 [input]
  (let [max-node (+ 3 (apply max input))
        data (rev-sort (conj input 0))
        initial (hash-map max-node 1)
        graph (reduce step initial data)]
    (get graph 0)))

(assert (= 8 (ans-2 test-data-1)))
(assert (= 19208 (ans-2 test-data-2)))

(comment
  ; Answer 2 -> 3454189699072
  (ans-2 real-data))

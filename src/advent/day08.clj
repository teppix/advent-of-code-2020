(ns advent.day08
  (:require [advent.util :refer [get-resource]]
            [clojure.string :as str]))

(def test-data (get-resource "test"))
(def real-data (get-resource "real"))

(defn parse-prg [s]
  (->> s
       str/split-lines
       (mapv #(str/split % #" "))
       (mapv (fn [[c a]] [c (Integer/parseInt a)]))))

(defn run [trace? prg]
    (loop [steps 0
           vis #{}
           [pc acc] [0 0]]

      (let [[cmd arg] (nth prg pc nil)]
        (cond
          ;; infinite loop
          (contains? vis pc)
          (do
            (when trace? (printf "infinite loop detected after %d steps\n" steps))
            {:acc acc :ok false})

          ;; graceful exit
          (nil? cmd)
          (do
            (when trace? (printf "program exited after %d steps\n" steps))
            {:acc acc :ok true})

          ;; running
          :else
          (do
            (when trace? (printf "[%s %2d] pc=%d acc=%d\n" cmd arg pc acc))
            (recur (inc steps) (conj vis pc)
                   (case cmd
                     "nop" [(inc pc) acc]
                     "acc" [(inc pc) (+ acc arg)]
                     "jmp" [(+ pc arg) acc]
                     )))))))

;; Part 1

(assert (= {:acc 5 :ok false} (run false (parse-prg test-data))))
(comment
  ; Answer 1
  (run false (parse-prg real-data))
  )

;; part 2

(defn modify [addr prg]
  (update prg addr
          (fn [[cmd arg]]
            (case cmd
              "acc" [cmd arg]
              "nop" ["jmp" arg]
              "jmp" ["nop" arg]))))

(defn run-mod [trace? poke-at prg]
  (->> prg
       (modify poke-at)
       (run trace?)))

(assert (= [8 true]
           (->> test-data
                parse-prg
                (run-mod false 7))))

(defn fix-prg [src]
  (let [prg (parse-prg src)]
    (loop [addr 0]
      (let [{:keys [acc ok]} (run-mod false addr prg)]
        (if ok {:fix addr :acc acc}
               (recur (inc addr)))))))

(assert (= 7 (:fix (fix-prg test-data))))

(comment
  ; Answer 2
  (fix-prg real-data))

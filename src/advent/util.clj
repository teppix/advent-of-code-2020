(ns advent.util
  (:require [clojure.string :as str]))

(defn get-resource [suffix]
  (let [base-name (.getName (clojure.java.io/file *file*))
        day (first (str/split base-name #"\."))
        filename (str day "-" suffix ".txt")
        path (clojure.java.io/resource filename)]
    (slurp path)
    ))

(defn multi-split [data sep]
  (if (empty? sep)
    data
    (map #(multi-split % (rest sep)) (str/split data (first sep)))))

(defn split-groups [data] (multi-split data [#"\n\n" #"\n"]))

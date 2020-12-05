(ns advent.day04
  (:require [advent.util :refer [get-resource]]
            [clojure.string :as str]
            [instaparse.core :as insta]))

(def test-data (get-resource "test"))
(def real-data (get-resource "real"))

(def parse
  (insta/parser
     "root = (chunk <lf lf+>)* chunk
      chunk = (attr <(s|lf)>)* attr
      attr = #'\\w+' <':'> #'[#\\w]+'
      lf = '\n'
      s = ' '"))

(defn check-chunk-v1 [chunk]

  (let [requirements ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"]
        keys (set (map second (rest chunk)))]
    (every? keys requirements)))


(defn chunk-to-map [chunk]
  (apply hash-map (apply concat (map rest (rest chunk)))))

(defn to-int [val]
  (try
    (Integer/parseInt val)
    (catch Exception _ nil)))

(defn between [& args]
  (try
    (apply <= args)
    (catch Exception e
      false
      )))

(defn check-chunk-v2 [chunk]
  (try
    (let [get-str (chunk-to-map chunk)
          get-num #(Integer/parseInt (get-str %))
          re #(re-matches %2 (get-str %1))
          re? #(some? (apply re %&))
          ]
      (and
        (re? "byr" #"^\d{4}$")
        (<= 1920 (get-num "byr") 2002)
        (re? "iyr" #"^\d{4}$")
        (<= 2010 (get-num "iyr") 2020)
        (re? "eyr" #"^\d{4}$")
        (<= 2020 (get-num "eyr") 2030)
        (or (between 150 (to-int (second (re "hgt" #"^(\d+)cm$"))) 193)
            (between 59 (to-int (second (re "hgt" #"^(\d+)in$"))) 76))
        (re? "hcl" #"^#[0-9a-f]{6}$")
        (re? "ecl" #"^(amb|blu|brn|gry|grn|hzl|oth)$")
        (re? "pid" #"^\d{9}$")
        )
      )
    (catch Exception e
      false
      ))
  )

(defn check-chunks [validate root]
  (map validate (rest root)))

(assert (= [true false true false]
           (check-chunks check-chunk-v1 (parse test-data))))

(comment
  ; Answer 1
  (count (filter check-chunk-v1 (rest (parse real-data))))
  ; Answer 2
  (count (filter check-chunk-v2 (rest (parse real-data)))))


(comment
  ; valid
  (map check-chunk-v2 (rest (parse "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980\nhcl:#623a2f\n\neyr:2029 ecl:blu cid:129 byr:1989\niyr:2014 pid:896056539 hcl:#a97842 hgt:165cm\n\nhcl:#888785\nhgt:164cm byr:2001 iyr:2015 cid:88\npid:545766238 ecl:hzl\neyr:2022\n\niyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719")))
  ; invalid
  (map check-chunk-v2 (rest (parse "eyr:1972 cid:100\nhcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926\n\niyr:2019\nhcl:#602927 eyr:1967 hgt:170cm\necl:grn pid:012533040 byr:1946\n\nhcl:dab227 iyr:2012\necl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277\n\nhgt:59cm ecl:zzz\neyr:2038 hcl:74454a iyr:2023\npid:3556412378 byr:2007")))
  )


(ns advent.day04
  (:require [advent.util :refer [get-resource]]
            [instaparse.core :as insta]))

;; Data

(def test-data (get-resource "test"))
(def real-data (get-resource "real"))

;; Common

(defn check [check-entry data]
  (case (first data)
    :root (count (filter (partial check check-entry) (rest data)))
    :entry (check-entry (rest data))))

(def required ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"])

;; V1

(def parse-v1
  (insta/parser
    "root = (entry <lf lf>)* entry
     entry = (param <(s|lf)>)* param
     <param> = #'\\w+' <#':[^\\s]*'>
     <lf> = '\n'
     <s> = ' '"))

(defn check-entry-v1 [params]
  (let [pset (set params)]
    (every? #(pset %) required)
    ))

(defn check-v1 [data]
  (check check-entry-v1 (parse-v1 data)))

;; V2

(def parse-v2
  (insta/parser
     "root = (entry <lf lf>)* entry
      entry = (param <(s|lf)>)* param
      <param> = #'[^\\s]+' | byr | iyr | eyr | hgt | hcl | ecl | pid
      byr = <'byr:'> #'\\d{4}'
      iyr = <'iyr:'> #'\\d{4}'
      eyr = <'eyr:'> #'\\d{4}'
      hgt = <'hgt:'> #'\\d+' ('cm'|'in')?
      hcl = <'hcl:'> #'#[0-9a-f]{6}'
      ecl = <'ecl:'> ('amb'|'blu'|'brn'|'gry'|'grn'|'hzl'|'oth')
      pid = <'pid:'> #'\\d{9}'
      <lf> = '\n'
      <s> = ' '"))

(defn check-param-v2 [key [val arg]]
  (if (nil? val)
    ; parameter is missing
    false
    ; validate parameter
    (let [get-int #(Integer/parseInt val)]
      (case key
        :byr (<= 1920 (get-int) 2002)
        :iyr (<= 2010 (get-int) 2020)
        :eyr (<= 2020 (get-int) 2030)
        :hgt (case arg
               "cm" (<= 150 (get-int) 193)
               "in" (<= 59 (get-int) 76)
               false)
        :hcl true
        :ecl true
        :pid true
        ))))

(defn entry-to-map [params]
  (reduce
    (fn [coll param] (assoc coll (first param) (rest param)))
    {} params))

(defn check-entry-v2 [params]
  (let [m (entry-to-map params)]
    (every? #(check-param-v2 % (m %)) (map keyword required))))

(defn check-v2 [data]
  (check check-entry-v2 (parse-v2 data)))

;; Assertions

(assert (= 2 (check-v1 test-data)))
(assert (= 2 (check-v2 test-data)))

(assert (= 4 (check-v2 "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980\nhcl:#623a2f\n\neyr:2029 ecl:blu cid:129 byr:1989\niyr:2014 pid:896056539 hcl:#a97842 hgt:165cm\n\nhcl:#888785\nhgt:164cm byr:2001 iyr:2015 cid:88\npid:545766238 ecl:hzl\neyr:2022\n\niyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719")))
(assert (= 0 (check-v2 "eyr:1972 cid:100\nhcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926\n\niyr:2019\nhcl:#602927 eyr:1967 hgt:170cm\necl:grn pid:012533040 byr:1946\n\nhcl:dab227 iyr:2012\necl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277\n\nhgt:59cm ecl:zzz\neyr:2038 hcl:74454a iyr:2023\npid:3556412378 byr:2007")))

;; Answer

(comment
  ; Answer 1
  (check-v1 real-data)
  ; Answer 2
  (check-v2 real-data))

(ns core.core
  (:require [clojure.data.json :as json]))
;Functional Programming Task one
;             ● Does it produce a correct answer
;             ● Use of functions
;             ● Use of data structures
;             ● Use of list comprehensions
;             ● Appropriate recursion
;             ● Pattern matching
;             ● Use of Clojure specs
;             ● Immutable data throughout
;             ● Naming of functions and vars

;Squaring Lists Sub-task 1
(defn square_list [n]
  (lazy-seq
    (if (not (empty? n))
      (if (number? (first n))
        (cons (* (first n)(first n)) (square_list (rest n)))
        (square_list (rest n))
))))

(defn map_square [n]
  (map #(* % %) (filter number? n))
)
(print (map_square [1 2 3 4 5 6 7 8 9]))

;Counting coins Sub-task 2

(defn denomination_count
  (memoize
    (fn [amount, dens]
      (cond
        (or (empty? dens)(> 0 amount)) 0
        (== amount 0) 1
        :else (+ (denomination_count amount (rest dens))(denomination_count (- amount (first dens)) dens))
        ))))
(defn dollar_count [amount, dens]
  (time (denomination_count amount, (sort < dens)))
)
(println (dollar_count 100 [1 5 10 25]))

(defn dollar_change [amount, dens]

  )
(dollar_change 100 [25, 10, 5, 1])

;Kindergardeners Sub-task 3

(def children ["Alice" "Bob" "Charlie" "David" "Eve" "Fred" "Ginny"
               "Harriet" "Ileana" "Joseph" "Kincaid" "Larry"])
(def plants ["Violets" "Grass" "Clover" "Radishes"])
(def shelfs "VRCGVVRVCGGCCGVRGCVCGCGVVRCCCGCRRGVCGCRVVCVGCGCV")

(defn transform ;gives a key of the starting letter to each item in the list, used to identify plants
  [coll]
  (reduce-kv (fn [m k v]
               (if (empty? v)
                 m
                 (assoc m (keyword (str(first v))) v)))
             {}
             coll))
(defn match-plants [children plants shelfs]
  (def key-plants (transform plants))
  (def first-shelf(subs shelfs 0 (/(count shelfs)2)))
  (def second-shelf(subs shelfs (/(count shelfs)2) (count shelfs)))
  (map-indexed
    (fn [i v] {(keyword v)
               [(key-plants (keyword(str(nth first-shelf (* i 2)))))
                (key-plants (keyword(str(nth first-shelf (+(* i 2)1)))))
                (key-plants (keyword(str(nth second-shelf (* i 2)))))
                (key-plants (keyword(str(nth second-shelf (+(* i 2)1)))))]})
    children)
  )
(defn kindergardeners [children plants shelfs]
  (if (= (/ (count shelfs) (count children)) 4) ; Each child gets 4 cups, two on each row
    (match-plants children plants shelfs)
    "Wrong number of children per plant"
    ))
(kindergardeners children plants shelfs)


;Meteor falls Sub-task 4

(def info (json/read-str (slurp "https://data.nasa.gov/resource/y77d-th95.json") :key-fn keyword))

;1. Which year saw the most individual meteor falls?
()

;2. Which year saw the heaviest collective meteor fall?

(ns core.core
  (:require [clojure.test :refer :all])
  (:require [clojure.data.json :as json])
  (:require [clojure.string :as str]))
(use 'clojure.test)
;:dependencies [[org.clojure/clojure "1.10.0"]
;                 [org.clojure/data.json "0.2.7"]]

;Squaring Lists Sub-task 1

;Explanation
;There many ways to produce lazy sequences in clojure (e.g. range, take, repeat, iterate, lazy-seq)
(defn square_list [n]
  (lazy-seq
    (if (not (empty? n))
      (if (number? (first n))
        (cons (* (first n)(first n)) (square_list (rest n)))
        (square_list (rest n))
        ))))

;The map function returns a lazy sequence and are well known in functional programming languages for being powerful
;The filter function is used to remove non-numerical items
;Then the map function applies the anonymous function #(* % %) (where % is the current list item) to each item
;Finally a lazy sequence of the squared list items is returned

(defn map-square [n]
  (map #(* % %) (filter number? n)))
(map-square [1 2 3 4 5 6 7 8 9])

;Testing section
(deftest square-list-test
  (testing "Square list"
    (testing "will return a lazy sequence"
      (is true (instance? clojure.lang.LazySeq (map-square [1 2 3 4 5 6 7 8 9]))))
    (testing "with all forms of numbers."
      (is (= [1 4 9 16 25 36 49 64 81] (map-square [1 2 3 4 5 6 7 8 9])))
      ;using decimals, exponents, hexadecimal
      ;in denary the numbers being squared are [0.525, 300, 15]
      (is (= [0.275625 90000.0 225] (map-square [0.525 3e2 0xF]))))
    (testing "with invalid data"
      (is (= [4] (map-square [2 "r" (sorted-map) "forty"])))
      (is (= [] (map-square ["qwerty" "m"])))
      (is (Thrown? IllegalArgumentException (map-square 4))))))


;Counting coins Sub-task 2

;To calculate the number of ways to make change for an amount equals
;Ways to make amount (A) using all but the first kind of coin plus...
;Ways to change amount A − D (Denominations) using all n kinds of coins
;Once the amount gets to 0 then a new combination of change is made and
(defn denomination-count [amount, dens]
  (cond
    (or (empty? dens)(> 0 amount)) 0
    (== amount 0) 1
    :else (+ (denomination-count amount (rest dens))
             (denomination-count (- amount (first dens)) dens))))
;Rebind denomination-count to a memoised version for caching
(def denomination-count (memoize denomination-count))
(defn dollar_count
  [amount coins]
  ;This map function caches each coin amount from 0, 1, 2,....n+1
  ;This means that each previous amount become memoised and subsequent amounts
  ;speed up exponentially as all amounts but the current amount have been cached
  ;I have tried many steps in trying to memoise in steps of 1, 100 and 1000 and have timed each in a new REPL
  ;Incrementing by 1000 can results in a less stable system and can cause stack overflow errors
  ;Incrementing by 1 is slightly slower than 10 while 100 is faster than both in large amounts (e.g. 100000 cents)
  ;On smaller amounts (e.g. 242) it makes sense to keep it incrementing by 1 or at a push 10 as incrementing 100 or 1000
  ;on 300 for example loses opportunities to optimise (albeit not needed really on smaller amounts)
  (last (map #(denomination-count % coins) (range (inc amount)))))
; (last (map #(denomination-count % coins) (range 0 (inc amount) 10))))
; (last (map #(denomination-count % coins) (range 0 (inc amount) 100))))
; (last (map #(denomination-count % coins) (range 0 (inc amount) 1000))))

(time(dollar_count 100 [1 5 10 25]))
(time(dollar_count 100000 [1 5 10 25 50 100]))

(deftest coin-count-test
  (testing "Coin count denominations"
    (testing "with valid data"
      (is (= 242 (dollar_count 100 [1 5 10 25])))
      (is (= 13398445413854501 (dollar_count 100000 [1 5 10 25 50 100]))))))



;Kindergardeners Sub-task 3

(def children ["Alice" "Bob" "Charlie" "David" "Eve" "Fred" "Ginny"
               "Harriet" "Ileana" "Joseph" "Kincaid" "Larry"])
(def plants ["Violets" "Grass" "Clover" "Radishes"])
(def shelfs "VRCGVVRVCGGCCGVRGCVCGCGVVRCCCGCRRGVCGCRVVCVGCGCV")

(defn transform ;gives a key of the starting letter to each item in the list, used to identify plants
  [coll]
  (reduce-kv (fn [m k v]
               (if (empty? v) m
                              (assoc m (keyword (str(first v))) v)))
             {} coll))
(defn match-plants [children plants shelfs cups-per-row]
  (def key-plants (transform plants))
  (reduce into (sorted-map)
          (map-indexed
            (fn [i v] {(keyword v)
                       (map
                         #(key-plants (keyword(str %)))
                         (reduce into [] (map
                                           #(take cups-per-row
                                                  (drop (* cups-per-row i) %))
                                           shelfs)))})
            children)))
(defn kindergardeners [children plants shelfs no-of-shelfs cups-per-row]
  (if (= (/ (count shelfs) (count children)) (* cups-per-row no-of-shelfs)) ; Each child gets 4 cups, two on each row
    (match-plants children plants
                  (partition (/ (count shelfs)no-of-shelfs) shelfs)
                  cups-per-row)
    "Wrong number of children per plant"))
(kindergardeners children plants shelfs 2 2)

(deftest coin-count-test
  (testing "Coin count denominations"
    (testing "with valid data"
      (is (= 242 (dollar_count 100 [1 5 10 25])))
      (is (= 13398445413854501 (dollar_count 100000 [1 5 10 25 50 100]))))))

;Meteor falls Sub-task 4
(defn keywordify [string]
  (keyword(str/replace string #"[\)\(\s\,\'\n\/\:\@']" "")))
(defn my-value-reader [key value]
  (if (= key :date)
    (java.sql.Date/valueOf value)
    value))
(defn define-data [info]
  (cond
    (string? info) (json/read-str (slurp info)
                                  :value-fn my-value-reader
                                  :key-fn keyword)
    (coll? info) info))
(def info (json/read-str
            (slurp "https://data.nasa.gov/resource/y77d-th95.json")
            :value-fn my-value-reader
            :key-fn keyword))
(defn parse-date-year [x]
  (.format
    (java.text.SimpleDateFormat. "yyyy")
    (.parse
      (java.text.SimpleDateFormat. "yyyy-MM-DD") x)))
(defn frequencies-by-key [key data]
  (frequencies (for [x data]
                 (if (contains? x key)
                   (x key)))))

;1. Which year saw the most individual meteor falls?
(defn most-falls-year [data]
  (def d (define-data data))
  (->>
    (reduce (fn [m [d v]]
              (if (some? d)
                (assoc m (parse-date-year d) v)))
            {} (frequencies-by-key :year d))
    (sort-by val)
    last))
(most-falls-year info)

;2. Which year saw the heaviest collective meteor fall?
(defn heaviest-collective-fall [data]
  (->>
    (apply merge-with (comp +)
           (for [x data]
             (if (and (contains? x :mass)(contains? x :year))
               {(keyword (parse-date-year (x :year)))
                (Float/parseFloat (x :mass))})))
    (sort-by val)
    last
    ))
(heaviest-collective-fall info)

;3. Which type of meteor is the heaviest on average?
(defn heaviest-type [data]
  (def meteorites (filter some? (map
                                  #(if (and (contains? % :recclass)(contains? % :mass))
                                     {(keyword(keywordify (% :recclass)))
                                      (Float/parseFloat (% :mass))})data)))
  (def meteorites-freq (frequencies(map
                                     #(if (and (contains? % :recclass)(contains? % :mass))
                                        (keyword(keywordify (% :recclass)))) data)))
  (->>
    (reduce
      (fn [m [k v]] (assoc m k (/ v (meteorites-freq k))))
      {} (apply merge-with (comp +) meteorites))
    (sort-by val)
    last)
  )
(heaviest-type info)


;4. Which two meteorites are closest together?
(defn distance-two-points [[x1 y1] [x2 y2]]
  (let [dx (- x2 x1), dy (- y2 y1)]
    (Math/sqrt (+ (* dx dx) (* dy dy)))))
(defn closest-pair [data]
  (def vec-co-ords (filter some?(into [] (map
                                           #(if (and (contains? % :name)
                                                     (contains? % :geolocation)
                                                     (= (% :fall) "Fell"))
                                              {(keyword (str/replace (% :name) #"[\)\(\s\,\'\n\/']" ""))
                                               ((% :geolocation):coordinates)})
                                           data))))
  (loop [i 0 j 1 dist 10000000]
    (println i j dist)
    (if (= i (dec (count vec-co-ords)))
      dist
      (let [new-dist (min dist (distance-two-points
                                 (val(first(nth vec-co-ords i)))
                                 (val(first(nth vec-co-ords j)))))]
        (if (= j (dec (count vec-co-ords)))
          (recur (inc i) (-> i inc inc) new-dist)
          (recur i (inc j) new-dist))))))
(closest-pair info)

;5. Find how many years between the first and latest meteorite
(defn meteorite-range [data]
  (def years (filter some? (map
                             #(if (and (contains? % :year))
                                (Integer/parseInt(parse-date-year (% :year))))data)))
  (- (apply max years) (apply min years)))
(meteorite-range info)


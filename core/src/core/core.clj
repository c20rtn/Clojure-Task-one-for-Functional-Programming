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
;Here is a not so elegant way of doing it through lazy seq recursively
(defn square-list [n]
  (lazy-seq
    (if (not (empty? n))
      (if (number? (first n))
        (cons (* (first n)(first n)) (square-list (rest n)))
        (square-list (rest n))))))

;The map function returns a lazy sequence and are well known in functional programming languages for being powerful
;The filter function is used to remove non-numerical items
;Then the map function applies the anonymous function #(* % %) (where % is the current list item) to each item
;Finally a lazy sequence of the squared list items is returned
(defn map-square [n]
  (map #(* % %) (filter number? n)))
(map-square [1 2 3 4 5 6 7 8 9])

;Testing section
(deftest square-list-test
  (testing "Square list using map"
    (testing "will return a lazy sequence"
      (is true (instance? clojure.lang.LazySeq (square-list [1 2 3 4 5 6 7 8 9]))))
    (testing "with all forms of numbers."
      (is (= [1 4 9 16 25 36 49 64 81] (square-list [1 2 3 4 5 6 7 8 9])))
      ;using decimals, exponents, hexadecimal
      ;in denary the numbers being squared are [0.525, 300, 15]
      (is (= [0.275625 90000.0 225] (square-list [0.525 3e2 0xF]))))
    (testing "with invalid data"
      (is (= [4] (square-list [2 "r" (sorted-map) "forty"])))
      (is (= [] (square-list ["qwerty" "m"])))))
  (testing "Square list using map"
    (testing "will return a lazy sequence"
      (is true (instance? clojure.lang.LazySeq (map-square [1 2 3 4 5 6 7 8 9]))))
    (testing "with all forms of numbers."
      (is (= [1 4 9 16 25 36 49 64 81] (map-square [1 2 3 4 5 6 7 8 9])))
      ;using decimals, exponents, hexadecimal
      ;in denary the numbers being squared are [0.525, 300, 15]
      (is (= [0.275625 90000.0 225] (map-square [0.525 3e2 0xF]))))
    (testing "with invalid data"
      (is (= [4] (map-square [2 "r" (sorted-map) "forty"])))
      (is (= [] (map-square ["qwerty" "m"]))))))


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
(defn dollar-count
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

(time(dollar-count 100 [1 5 10 25]))
(time(dollar-count 100000 [1 5 10 25 50 100]))

(deftest coin-count-test
  (testing "Coin count denominations"
    (testing "with valid data"
      (is (= 242 (dollar-count 100 [1 5 10 25])))
      (is (= 13398445413854501 (dollar-count 100000 [1 5 10 25 50 100]))))
    (testing "with valid data"
      (is (= 0 (dollar-count 100 [])))
      (is (= 1 (dollar-count 0 [1 5 10 25 50 100]))))))


;Kindergardeners Sub-task 3

(def children ["Alice" "Bob" "Charlie" "David" "Eve" "Fred" "Ginny"
               "Harriet" "Ileana" "Joseph" "Kincaid" "Larry"])
(def plants ["Violets" "Grass" "Clover" "Radishes"])
(def shelfs "VRCGVVRVCGGCCGVRGCVCGCGVVRCCCGCRRGVCGCRVVCVGCGCV")

(defn transform ;gives a key of the starting letter to each item in the list, used to identify plants using initials
  [coll]
    (reduce-kv (fn [m k v]
     (if (empty? v) m
        (assoc m (keyword (str/capitalize(str(first v)))) v)))
    {} coll))
(defn match-plants [children plants shelfs cups-per-row]
  (def key-plants (transform plants)) ;Use the initials of each plant as a key so they can easily referenced
  (reduce into (sorted-map)
          (map-indexed
            (fn [i v] {(keyword v) ;Childs name will be the keyword in the returned map as to easily attain their plants
                       (map
                         #(key-plants (keyword(str/capitalize(str %)))) ;replaces each plant letter with plant name
                         (reduce into [] (map ;Gets the corresponding cups for that child from each shelf into a coll
                                           #(take cups-per-row (drop (* cups-per-row i) %)) ;Uses index to drop and take plants
                                           shelfs)))})
            children)))
(defn kindergardeners [children plants shelfs no-of-shelfs cups-per-row]
  (if (= (count shelfs) (* (count children) cups-per-row no-of-shelfs)) ; Checks to see if the parameters add up
    (match-plants (sort-by str (map str/capitalize children))  ;Sorts children in alphabetical order as per the spec
                  plants
                  (partition (/ (count shelfs)no-of-shelfs) shelfs) ;Split the shelfs into the match plants function so that shelfs can be used on a map function
                  cups-per-row)
    "Wrong number of cups per row and/or number of shelfs"))

(deftest kindergardeners-test
  (testing "Kindergardeners"
    (testing "with valid data"
      ;Gives answers according to the question
      (is (= ["Violets" "Radishes" "Violets" "Radishes"] (:Alice (kindergardeners children plants shelfs 2 2))))
      (is (= ["Clover" "Grass" "Clover" "Clover"] (:Bob (kindergardeners children plants shelfs 2 2))))
      ;If changes the cups per row to 4 and shelfs to 1 then people get sequential sets of 4 plants
      (is (= ["Violets" "Radishes" "Clover" "Grass"] (:Alice (kindergardeners children plants shelfs 1 4))))
      (is (= ["Violets" "Violets" "Radishes" "Violets"] (:Bob (kindergardeners children plants shelfs 1 4))))
      ;Different plants and children
      (is (= ["Chamomile" "Strawberry" "Chamomile" "Poppy" "Apple" "Rice"] (:Mya (kindergardeners ["Brayan" "August" "Brianna" "Mya" "Salma" "Barrett" "Mallory" "Jazlynn" "Julius" "Aliya"
                                                                       "Katelyn" "Dereon" "Courtney" "Christian" "Charity" "Erica" "Miya" "Javon" "Delilah" "Jaiden"]
                                                                      ["Rice" "Dandelions" "Ivy" "Birch" "Apple" "Poppy" "Chamomile" "Strawberry"]
                                                                      "IBIRDSPBIBRBPISBPBICCIPDPRIBABICAPSICSDPRRBARDBRPRBADRBBPBCRACBSRIAPBIRPIRBBCPSDSAIRRIPDDBSCIIBAACSAASIDBBDISRBCPAABARCR"
                                                                      3 2)))))
    (testing "alternative scenarios"
      ;Two of the same name will only count last instance of the name
      (is (= ["Clover" "Grass" "Clover" "Clover"] (:Alice (kindergardeners ["Alice" "Alice" "Bob" "Charlie" "David" "Eve" "Fred" "Ginny"
                                                                                   "Harriet" "Ileana" "Joseph" "Kincaid"] plants shelfs 2 2))))
      ;Names in wrong order
      (is (= ["Violets" "Radishes" "Violets" "Radishes"] (:Alice (kindergardeners ["Joseph" "Kincaid" "Larry""David" "Eve" "Fred" "Ginny"
                                                                               "Harriet" "Ileana""Charlie" "Alice" "Bob"] plants shelfs 2 2))))
      ;Different upper case and lower case shelfs, plants and names
      ;Use unsorted and randomly captilised names
      (is (= ["Violets" "Clover" "Violets" "Grass"] (:Joseph (kindergardeners ["joseph" "Kincaid" "Larry" "David" "Eve" "Fred" "Ginny"
                                                                                "Harriet" "Ileana" "charlie" "Alice" "bob"] plants shelfs 2 2))))
      ;Randomly captilised list of childrens plants should be same as all capitilised list of plants
      (is (= (kindergardeners children plants "vrcGvvrvCGGccgvRgcVcgCGVvrcccGcRrGvcGCRvVcVgcgCV" 2 2) (kindergardeners children plants shelfs 2 2)))
      ;Still gives names for plant even if non capitlised
      (is (= ["Violets" "Clover" "Violets" "grass"] (:Joseph (kindergardeners children ["Violets" "grass" "Clover" "radishes"] shelfs 2 2)))))
    (testing "with invalid or potentially breaking data"
      ;No shelfs
      (is (= "Wrong number of cups per row and/or number of shelfs" (kindergardeners children plants shelfs 0 2)))
      ;Shelf includes plants not in the list gives nil values
      (is (= ["Violets" nil "Violets" "Radishes"] (:Alice (kindergardeners children plants "VICGVVRVCGGICFVRGCVCFCGVVRCCCGCRIGVFGCRVVCVFCGCV" 2 2))))
      ;No plants means everyone has a collection of nils
      (is (= [nil nil nil nil] (:Alice (kindergardeners children [] shelfs 2 2))))
      ;No children/shelf of plants means the program will exit early
      (is (= "Wrong number of cups per row and/or number of shelfs" (kindergardeners children plants [] 2 2)))
      (is (= "Wrong number of cups per row and/or number of shelfs" (kindergardeners [] plants shelfs 2 2))))))



;Meteor falls Sub-task 4

;Used to keyword data that may have brackets or spaces that cause them to not keyword properly
(defn keywordify [string]
  (keyword(str/replace string #"[\)\(\s\,\'\n\/\:\@']" "")))

;Reads the JSON file and handles dates
(defn my-value-reader [key value]
  (if (= key :date)
    (java.sql.Date/valueOf value)
    value))

;user can enter the URL or full collection into each function for flexibility
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
;Used to parse the dates that are given in the NASA JSON file
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
                (assoc m (parse-date-year d) v))) ;Parses the date just to show the year
            {}
            (frequencies-by-key :year d)) ;Gets the frequencies of each year
    (sort-by val)
    last))
(most-falls-year info)

(deftest Q4-1-test
  (testing "Question 4-1. Which year saw the most individual meteor falls?"
    ;Test using the previously downloaded data
    (is (=["1933" 16]
          (most-falls-year info)))
    ;Test using the URL
    (is (=["1933" 16]
          (most-falls-year "https://data.nasa.gov/resource/y77d-th95.json")))))


;2. Which year saw the heaviest collective meteor fall?
(defn heaviest-collective-fall [data]
  (def d (define-data data))
  (->>
    ;this merge with function takes each entry in the coll and merges them based on duplicate keys (the year)
    (apply merge-with (comp +)
           ;Gets mass of every year in a coll
           (for [x d]
             (if (and (contains? x :mass)(contains? x :year))
               {(keyword (parse-date-year (x :year))) ;parses the date to get just the year
                (Float/parseFloat (x :mass))})))
    (sort-by val)
    last
    ))
(heaviest-collective-fall info)

(deftest Q4-2-test
  (testing "Question 4-2. Which year saw the heaviest collective meteor fall?"
    ;Test using the previously downloaded data
    (is (=[:1947 2.303023E7]
          (heaviest-collective-fall info)))
    ;Test using the URL
    (is (=[:1947 2.303023E7]
          (heaviest-collective-fall "https://data.nasa.gov/resource/y77d-th95.json")))))


;3. Which type of meteor is the heaviest on average?
(defn heaviest-type [data]
  (def d (define-data data))
  ;Gets a collection of meteorite entry with the class as the key and the mass as the value
  (def meteorites (filter some? (map
                                  #(if (and (contains? % :recclass)(contains? % :mass))
                                     {(keywordify (% :recclass))
                                      (Float/parseFloat (% :mass))})d)))
  ;Gets a collection each meteorite class and how many of each there are
  (def meteorites-freq (frequencies(map
                                     #(if (and (contains? % :recclass)(contains? % :mass))
                                        (keywordify (% :recclass))) d)))
  (->>
    (reduce
      (fn [m [k v]] (assoc m k (/ v (meteorites-freq k))))  ;divide each collective weight of meteor by how many times it occurs to get average
      {} (apply merge-with (comp +) meteorites)) ;get collective weight of each meteor class using comp and merge-with
    (sort-by val)
    last)
  )
(heaviest-type info)

(deftest Q4-3-test
  (testing "Question 4-3. Which type of meteor is the heaviest on average?"
    ;Test using the previously downloaded data
    (is (=[:IronIIAB 3885195.3333333335]
          (heaviest-type info)))
    ;Test using the URL
    (is (=[:IronIIAB 3885195.3333333335]
          (heaviest-type "https://data.nasa.gov/resource/y77d-th95.json")))))


;4. Which two meteorites are closest together?
(defn distance-two-points [[x1 y1] [x2 y2]] ;Use pythagoras to find distance between two points
  (let [dx (- x2 x1), dy (- y2 y1)]
    (+ (* dx dx) (* dy dy))))
(defn closest-pair [data]
  (def d (define-data data))
  ;creates the co-ordinates for the function to run through
  (def vec-co-ords (filter some?(into [] (map
                                           #(if (and (contains? % :name)
                                                     (contains? % :geolocation)
                                                     (= (% :fall) "Fell"))
                                              {(keyword (str/replace (% :name) #"[\)\(\s\,\'\n\/']" "")) ((% :geolocation):coordinates)})
                                           d))))
  ;runs through all permutations of co-ordinates to find the lowest distance then square roots
  (Math/sqrt (loop [i 0 j 1 dist 100000000]
               ;(println i j dist)
               (if (= i (dec (count vec-co-ords)))
                 dist ;once i has hit the count of the co-ords then return distance
                 (let [new-dist (min dist (distance-two-points ;finds the min between the new distance and the old and recurs with the lesser of the two
                                            (val(first(nth vec-co-ords i)))
                                            (val(first(nth vec-co-ords j)))))]
                   (if (= j (dec (count vec-co-ords)))  ;once the second co-ordinate has been ran through agaisnt the first then run again avoiding using the
                     (recur (inc i) (-> i inc inc) new-dist)
                     (recur i (inc j) new-dist)))))))

;Rather disappointingly the two closest are Galim (a) and Galim (b) which both landed in the exact same space at the same time apparently
(time (closest-pair info))

(deftest Q4-4-test
  (testing "Question 4-4. Which two meteorites are closest together?"
    ;Test using the previously downloaded data
    (is (= 0.0
           (closest-pair info)))
    ;Test using the URL
    (is (= 0.0
           (closest-pair "https://data.nasa.gov/resource/y77d-th95.json")))))

;5. Find how many years between the first and latest meteorite
(defn meteorite-range [data]
  (def d (define-data data))
  ;Get all the years in the JSON dataset
  (def years (filter some? (map
                             #(if (and (contains? % :year))
                                (Integer/parseInt(parse-date-year (% :year))))d)))
  (- (apply max years) (apply min years))) ;Using the max and min year we can find out the range of the dataset
(meteorite-range info)

(deftest Q4-5-test
  (testing "Question 4-5. Find how many years between the first and latest meteorite"
    ;Test using the previously downloaded data
    (is (= 1152
          (meteorite-range info)))
    ;Test using the URL
    (is (= 1152
          (meteorite-range "https://data.nasa.gov/resource/y77d-th95.json")))))

(time(run-tests))
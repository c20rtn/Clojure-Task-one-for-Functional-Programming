(ns core.core)
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

(defn denomination_count [amount, dens]
  (cond
    (or (empty? dens)(> 0 amount)) 0
    (== amount 0) 1
    :else (+ (denomination_count amount (rest dens))(denomination_count (- amount (first dens)) dens))
    )
  )
(defn dollar_count [amount, dens]
  (denomination_count amount, (sort < dens))
)
(println (dollar_count 100 [1 5 10 25]))

(defn dollar_change_reduce [amount, dens]


  )
(dollar_change_reduce 100 [25, 10, 5, 1])



(defn dollar_change [amount, dens]
  (if (and(<= 0 amount) (< 0 (count dens)))        ;
    (loop [_amount amount
           _dens (sort < dens)     ;can use any order and it will sort
           total 0
           den_index (- (count dens) 1)
           ]
      (println total)

      (cond
        (== 0 _amount) (recur
               amount
               _dens
               (inc total)
               (- (count dens) 1)) ;if the working amount is 0 then reset and inc the total
        (< 0 (quot _amount (get _dens den_index)))(recur
               (- _amount (*(quot amount (get _dens den_index))(get _dens den_index)))
               _dens
               total
               (dec den_index))
        :else (recur
                _amount
                _dens
                total
                (dec den_index))
))))
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

(defn kindergardeners [children plants shelfs]
  (if (/ (mod (count shelfs) (count children)) 4) ; Each child gets 4 cups, two on each row
    (def key-plants (transform plants))
    (def split-shelfs (split-at (/ (count shelfs) 2) (vec shelfs)))
    "Not correct amount of plants per child"))


(kindergardeners children plants shelfs)


;(nth(get (vec (split-at (/ (count shelfs) 2) (vec shelfs)))0)1)


(reduce-kv (fn [m k v]
             (if (empty? v)
               m
               (fn [children idx itm]
                 (assoc children (keyword v)
                                 [(nth(get new-shelfs 0)0) (nth(get new-shelfs 0)0) (nth(get new-shelfs 0)1) (nth(get new-shelfs 0)1)]))))
           {}
           children)




;Meteor falls Sub-task 4

(def info (slurp "https://data.nasa.gov/resource/y77d-th95.json"))

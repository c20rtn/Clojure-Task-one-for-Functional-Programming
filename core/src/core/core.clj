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

(println (square_list [1 2 3 4 5 6 7 8 9]))

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

(defn kindergardeners [children plants shelfs]
  (if (= (/ (count shelfs) (count children)) 4)
    (loop [_shelfs shelfs _children children]
      (print (first _children))
      (if (not-empty _children)
        (recur (shelfs) (rest _children))))
    "Not correct amount of plants per child"))

(kindergardeners children plants shelfs)

(defn transform
  [coll]
  (reduce (fn [ncoll [k v]]
            (assoc ncoll (keyword (str (first v))) v))
          {}
          coll))

(transform {:a "abc" :b "def" :c "ghi"})



;Meteor falls Sub-task 4
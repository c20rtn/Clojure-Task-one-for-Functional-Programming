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


;(defn dollar_change [amount, denominations]
;  (if (and(<= 0 amount) (< 0 (count denominations)))        ;
;    (loop [working_amount amount
;           working_denominations (sort < denominations)     ;can use any order and it will sort
;           total_denominations 0
;           denomination_index (- (count denominations) 1)
;           ]
;      (println total_denominations)
;
;      (cond
;        (== 0 working_amount) (recur
;                                amount
;                                working_denominations
;                                (inc total_denominations)
;                                (- (count denominations) 1)) ;if the working amount is 0 then reset and inc the total
;        (< 0 (quot working_amount (get working_denominations denomination_index)))(recur
;                                                                                     (- working_amount (*(quot amount (get working_denominations denomination_index))(get working_denominations denomination_index)))
;                                                                                     working_denominations
;                                                                                     total_denominations
;                                                                                     (dec denomination_index))
;        :else (recur
;                working_amount
;                working_denominations
;                total_denominations
;                (dec denomination_index))
;))))
;
;(dollar_change 100 [25, 10, 5, 1])

;Kindergardeners Sub-task 3




;Meteor falls Sub-task 4
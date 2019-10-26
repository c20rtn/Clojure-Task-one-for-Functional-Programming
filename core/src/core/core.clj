;Functional Programming Task one

;Squaring Lists Sub-task 1
(defn square_list [i limit]
  (lazy-seq
    (when (< i limit)
      (cons (* i i) (square_list (inc i) limit)))))

(square_list 0 50)





;Counting coins Sub-task 2




;Kindergardeners Sub-task 3




;Meteor falls Sub-task 4
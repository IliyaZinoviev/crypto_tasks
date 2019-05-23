(ns ex1.div)

;; polynomials division
;;
(defn get-max-term [map]
  (apply max-key key map))

(defn get-max-key [map]
  (first (get-max-term map)))

(defn get-max-val [map]
  (last (get-max-term map)))

(defn get-mult [t1 t2]
  [(- (first t1) (first t2))
   (/ (last t1) (last t2))])

(defn mult-m [p m]
  (reduce (fn [newcoll [k v]]
            (assoc newcoll (+ k (first m)) (* v (last m)))) {} p))

(defn sub [p1 p2]
  (apply hash-map (flatten
                   (filter (comp not zero? second)
                           (merge-with + p1 (reduce (fn [newcoll [k v]]
                             (assoc newcoll k (-' v))) {} p2))))))

(defn division-poly [dividend divisor]
  {:pre [(map? dividend) (map? divisor)
         (>= (get-max-key dividend) (get-max-key divisor))]}
  (let [max-term (get-max-term divisor)]
    (loop [remainder dividend  quotient {}]
      (let [mult (get-mult (get-max-term remainder) max-term)
            part-div (mult-m divisor mult)
            remainder (sub remainder part-div)
            quotient (assoc quotient (first mult) (last mult))]
        (if (> (get-max-key divisor) (get-max-key remainder))
          [remainder quotient]
          (recur remainder quotient))))))

(division-poly {3 1 2 -12 0 -42} {1 1 0 -3})
;; => [{0 -123} {2 1, 1 -9, 0 -27}]

;; (division-poly {3 1 2 -12 0 -42} {2 1 1 -2 0 1})
;; => [{0 -32, 1 -21} {1 1, 0 -10}]

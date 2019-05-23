(ns ex1.gf2
  (:require [clojure.math.combinatorics :as c]))
;; checking on Irreducible polynomial
;;
(defn check-keys-vals [poly]
  (empty? (filter (fn [[k v]]
                    (not (and (not (neg-int? k))
                              (or (= 0 v) (= 1 v)))))
                  poly)))

(defn get-max-term [map]
    (apply max-key key map))

(defn get-max-key [map]
  (first (get-max-term map)))

(defn get-max-val [map]
  (last (get-max-term map)))

(defn get-mult [t1 t2]
  [(- (first t1) (first t2)) 1])

(defn mult-m [p m]
  (reduce (fn [newcoll [k v]]
            (assoc newcoll (+ k (first m)) 1)) {} p))

(def sub (memoize (fn [p1 p2]
  (apply hash-map (flatten
                   (filter (comp not zero? second)
                           (merge-with bit-xor p1 p2)))))))

(def div (memoize (fn [dividend divisor]
     (let [max-term (get-max-term divisor)]
       (loop [remainder dividend  quotient {}]
         (let [mult (get-mult (get-max-term remainder) max-term)
               part-div (mult-m divisor mult)
               remainder (sub remainder part-div)
               quotient (assoc quotient (first mult) (last mult))]
           (if (or (empty? remainder)
                   (> (get-max-key divisor) (get-max-key remainder)))
             [remainder quotient]
             (recur remainder quotient))))))))

(defn get-polies [deg]
  (filter #(not= 0 (apply bit-xor %))
          (c/selections [1 0] deg)))

(defn irr? [func poly]
  (loop [poly poly irr (func)]
    (if (and irr (> (get-max-key poly) (get-max-key irr)))
      (let [[remainder quotient] (div poly irr)]
        (if (not (empty? remainder))
          (recur poly (func))
          false))
      true)))

(defn next-irr! []
  (let [s (atom {:i [{2 1 1 1 0 1}] :p nil :d 1})]
    (fn []
      (letfn [(polies []
                (when (empty? (:p @s))
                    (swap! s update-in [:d] inc)
                    (swap! s assoc :p (get-polies (:d @s))))
                  (let [poly (first (:p @s)) deg (inc (count poly))]
                    (swap! s update-in [:p] rest)
                    (merge {deg 1} (#(zipmap (range 1 deg) %) poly) {0 1})))
              (get-irr []
                  (let [irred (partial irr? (next-irr))]
                    (loop [p (polies)]
                      (if (irred p)
                        (do
                          (let [v (:i @s)]
                            (swap! s assoc :i (conj v p))
                            (v (dec (count v)))))
                        (recur (polies))))))
              (next-irr []
                (let [irrs (atom (:i @s))]
                  (fn []
                    (let [irr (first @irrs)]
                      (reset! irrs  (rest @irrs))
                      irr))))]
        (get-irr)))))

(defn div-x+1? [p]
  (zero? (apply bit-xor (map val p))))

(defn irreducible? [poly]
  {:pre [(map? poly)
         (not (zero? (count poly)))
         (check-keys-vals poly)]}
  (cond (or (and (> (count poly) 2) (zero? (get poly 0))) (div-x+1? poly)) false
        (< (count poly) 3) true
        :else (irr? (next-irr!) poly)))

;(irreducible? {8 1 4 1 0 1})

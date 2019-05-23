(ns ex1.psn
  (:require
   [clojure.string :as str]))
;; pseudorandom sequence of numbers
(defn check-pre [s]
  (and (string? s)
       (empty? (re-find #"[^0-1]" s))))

(defn get-last-els [size val]
  (subvec val (- (count val) size)))

(defn mult [vect op1 op2]
  (conj vect (apply bit-xor (map #(* %1 %2) op1 op2))))

(defn get-vect [s]
  (vec (map read-string (str/split s #""))))

(defn psn
  [polynomial initial-fill]
  {:pre [(= (count polynomial)  (count initial-fill))
         (check-pre polynomial) (check-pre initial-fill)]}
  (let [polynomial (get-vect polynomial) initial-fill (get-vect initial-fill)
        size (count polynomial) get-last-size (partial get-last-els size)]
    (loop [psn-val initial-fill last-els (get-last-size psn-val)]
      (if (and (> (count psn-val) size) (= last-els initial-fill))
        (str/join (reverse (nthrest (reverse psn-val) 4)))
        (do
          (println (str/join psn-val))
          (let [res (mult psn-val last-els polynomial)]
            (recur res (get-last-size res))))))))

(psn "1100" "1010")
;(psn "01010" "10101") ; здесь мне пришлось перезагрузить ноутбук((((((
(psn "1001" "1010")
(psn "10100" "10100")

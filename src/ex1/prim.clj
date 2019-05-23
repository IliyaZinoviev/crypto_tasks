(ns ex1.prim
  (:require [ex1.gf2 :refer [irreducible?
                             div
                             get-max-key]]))

(defn div? [poly deg]
  (let [[remainder _] (div poly {(inc (int (Math/pow 2 deg))) 1 0 1})]
    (empty? remainder)))

(defn undiv? [poly deg]
  (loop [n (inc deg)]
    (if (= n (int (Math/pow 2 deg)))
      true
      (if (div? poly n)
        false
        (recur (inc n)))
    )))

(defn primitive? [poly]
  (let [deg (get-max-key poly)]
    (and (irreducible? poly)
         (div? poly deg)
         (undiv? poly deg))))



(ns ex1.core
  (:require [clojure.string :as s]
            [ex1.div :refer [division-poly]]
            [ex1.psn :refer [psn]]
            [ex1.gf2 :refer [irreducible?]]
            [ex1.subs :refer [get-subs-size]]
            [ex1.prim :refer [primitive?]]))

(defn main []
  (do
    (println "pseudo-random number equal" (psn "1011" "1101"))
    (println (division-poly {3 1 2 -12 0 -42} {1 1 0 -3}))
    (println "irreducible?" (irreducible? {4 1 2 1 0 1}))
    (println "primitive?" (primitive? {4 1 2 1 0 1}))
    (get-subs-size (range 10))))



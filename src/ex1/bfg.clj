(ns ex1.bfg
  (:require [clojure.math.combinatorics :as c]
            [clojure.string :as s]))
;; bijective functions generator
;;
;;

(defn get-bin [n i]
  (let [len1 (count (Integer/toBinaryString n))
        bin (Integer/toBinaryString i)
        len2 (count bin)]
  (map #(if (= "0" % )  false true)
       (concat (repeat (- len1 len2) "0") (s/split bin #"")))))

(defn mapdef [fns args] (map #(%1 %2) fns args))

(defn get-var [bit]
  (if bit
    identity
    not))

(defn get-conj [bits]
  (let [vars (map get-var bits)]
    (fn [args]
       (every? identity (mapdef vars args)))))

(defn get-disj [conjs]
  (fn [args]
    (if (some identity (map #(% args) conjs))
      1
      0)))

(defn get-bin-fns [n get-bin]
  (let [conjs (map (comp get-conj get-bin)
                  (shuffle (range n)))]
    (loop [i 1 fns []]
      (if (> i n)
        fns
        (recur (inc i) (conj fns (get-disj (take i conjs))))))))

(defn bfg [n]
  {:pre [(> n 1)]}
  (let [gb (partial get-bin n)
        fns (get-bin-fns n gb)]
    (fn [i]
      (reduce + 0 (map #(% (gb i)) fns)))))

;(defn minimize )

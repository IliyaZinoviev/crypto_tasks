(ns ex1.test-mr
  (:require [clojure.math.numeric-tower :as math]))

(defn bin [n] (Integer/toString n 2))

(defn- sqr [x]
  (math/expt x 2))

(defn sqrt [n]
  (math/sqrt n))

(defn exp-pow [n k]
  (sqrt (reduce (fn [res val]
                  (sqr (if (= \0 val)
                         res
                         (*' res n))))
                1 (bin k))))

(defn repres-num [n]
  (loop [t (/ n 2) s 1]
    (if (zero? (mod t 2))
      (recur (/ t 2) (inc s))
      [t s])))

(defn rand [s n]
  (+ s (rand-int (- n s))))

(defn test-mr [n k]
  {:pre [(int n) (> n 3) (not (zero? (mod n 2)))]}
  (let [n-1 (dec n) [t s] (repres-num n-1)]
    (loop [k k]
      (if (= k 0)
      true
      (let [x (exp-pow (rand 2 n-1) t) y (mod x n)]
        (if (or (= y 1) (= y n-1))
          (recur (dec k))
          (if (loop [x (sqr x) y (mod x n) s2 (dec s)]
                (cond
                  (or (= y 1) (= s2 0)) false
                  (= y n-1) true
                  :else (let [x (sqr x)]
                          (recur x (mod x n) (dec s2)))))
            (recur (dec k))
            false)))))))

(test-mr 23521 4)



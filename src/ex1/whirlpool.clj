(ns ex1.whirlpool
  (:require [clojure.string :as s]
            [ex1.gf2 :as gf2]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.math.numeric-tower :as math]
            ))
;; Main data
;;
(def module {8 1 4 1 3 1 2 1 0 1})
(def sblock (with-open [r (io/reader "sbox.edn")]
              (edn/read (java.io.PushbackReader. r))))
(def mds-m (with-open [r (io/reader "mds.edn")]
             (edn/read (java.io.PushbackReader. r))))
;; (def k0 (with-open [r (io/reader "k0.edn")]
;;           (edn/read (java.io.PushbackReader. r))))
(defn gen-key []
  (apply str (repeatedly 512 #(rand-int 2))))
(def k0
  (apply str (repeat 512 0)))
;; Polynomial block
;;
(defn get-poly [s]
  (apply hash-map (flatten (filter (comp not zero? second)
                                   (zipmap (range (dec (count s)) -1 -1)
                                           (map #(Integer. (str %)) s))))))

(defn mod-gf [a]
  (if (or (empty? a) (< (gf2/get-max-key a) (gf2/get-max-key module)))
          a
          ((gf2/div a module) 0)))

(defn +gf [a b]
  (gf2/sub a b))

(defn *gf [a b]
  (if (or (empty? a) (empty? b))
    {}
    (reduce (fn [res x] (+gf res (gf2/mult-m a x))) {} b)))

(defn apply-fn-m [fn m]
  (map #(map fn %) m))

(defn transpose [m]
  (apply map vector m))

(defn apply-fn-ms [fn m1 m2]
  (map #(map fn %1 %2) m1 m2))

(defn nested-for [f x y]
  (map (fn [a]
         (map (fn [b]
                (f a b)) y))
       x))

(defn mult-gf-m [a b]
  (nested-for (fn [x y] (reduce +gf (map #(mod-gf (*gf % %2)) x y))) a
              (transpose b)))

;; Prepare data to hashing
;;
;; to get x 0s for M'
;; L = lenght(M); L2 = L + 1 + x; L2 === 0 mod 256 & L2 - odd
(defn get-x [len]
  (loop [c (inc len)]
    (if (and (zero? (mod c 256)) (= 1 (mod (/ c 256) 2)))
      (- (dec c) len)
      (recur (inc c)))))

(defn extend-mes [mes]
  (let [len (count mes) bin-len (Integer/toBinaryString len)]
    (s/join [mes 1 (s/join (repeat (+ (get-x len) (- 256 (count bin-len))) \0)) bin-len])))

(defn get-matr [str]
  (partition 8
   (re-seq #"\d{8}" str)))

(defn get-str [m]
  (reduce (fn [s poly]
            (s/join [s (let [number (Integer/toString
                                     (reduce (fn [res [k v]]
                                               (+ res (math/expt 2 k)))
                                             0 poly) 16)]
                         (if (= 1 (count number))
                             (s/join [0 number])
                           number))])) "" (flatten m)))

;; S-block
;;
(defn get-s [poly]
  (let [[i1 i2] (reduce (fn [[v1 v2] [k v]]
                          (if (< k 4)
                            [v1 (+ v2 (math/expt 2 k))]
                            [(+ v1 (math/expt 2 (- k 4))) v2]))
                        [0 0] poly)]
    ((sblock i1) i2)))
(defn sbox [m]
  (map #(map get-s %) m))
;; Cycling permutation
;;
(defn cyc-per [m]
  (transpose
   (map-indexed #(apply merge (subvec %2 % 8) (subvec %2 0 %))
                (transpose m))))
;; Linear diffusion
;;
(defn lin-diff [m]
  (mult-gf-m m mds-m))
;; Add key function
(defn add-key [k m]
  (apply-fn-ms gf2/sub m k))
;; Matrix xor
(defn m-xor [ms]
  (reduce (fn [m-new m] (apply-fn-ms #(gf2/sub %1 %2) m-new m)) (first ms) (rest ms)))

;; Get round const
(defn get-r-const [r j]
  (get-s (get-poly (Integer/toBinaryString (+ (* 8 (dec r)) j)))))

(defn get-const-m [r]
  (list* (map #(get-r-const r %) (range 0 8))
        (repeat 7 (repeat 8 {}))))

;; Round function
(defn r-fun [k m]
  (->> m
       sbox
       cyc-per
       lin-diff
       (add-key k)))

;; Get next round  key
(defn get-key [k r]
  (r-fun (get-const-m r) k))

;; Block cipher
(defn block-cipher [k m]
  (loop [i 0 k k m (add-key k m)]
    (if (< i 10)
      (let [i (inc i) k (get-key k i)]
        (recur i k (r-fun k m)))
      m)))

;; Compression function
(defn comp-fun [k m]
  (m-xor [(block-cipher k m) k m]))

;; Main function
;;
(defn to-bin [s]
  (reduce (fn [res c]
            (s/join [res (let [b (Integer/toBinaryString (int c)) len (count b)]
                           (if (= len 8)
                             b
                             (s/join [(apply str (repeat (- 8 (count b)) 0)) b])))])) "" s))

(defn get-blocks [s]
  (map (partial apply str) (partition 512 s)))

(defn get-m-polies [m]
  (apply-fn-m get-poly m))

(defn whirlpool [k0 s]
  (get-str (reduce (fn [k block]
            (->> block
                 get-matr
                 get-m-polies
                 (comp-fun k)))
      ((comp get-m-polies get-matr) k0) (get-blocks (extend-mes (to-bin s))))))

(whirlpool k0 "Whirlpool")


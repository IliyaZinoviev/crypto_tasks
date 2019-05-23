(ns ex1.subs
  (:require [clojure.java.io :as io]))


;; 1.4 get any i (P(a_i) != a_i)
(def cardinality (atom nil))
(def file (atom nil))

(defn without [set-nums pair]
  (filter #(and (not= (first pair) %) (not= (second pair) %)) set-nums))

(defn get-pairs [set]
  (let [f  (first set)]
    (pmap #(vector % f) (rest set))))

(defn make-map [func set]
  (into (sorted-map) (zipmap set (func set))))

(defn get-subs2 [el]
  (make-map reverse el))

(defn get-lst-subs2 [el]
  (list (get-subs2 el)))

(defn shift [el]
  (conj (vec (rest el)) (first el)))

(defn get-lst-subs3 [el]
   (list (make-map shift el) (make-map (comp shift shift) el)))

(defn make-subs [pair lst-maps]
  (pmap #(merge (get-subs2 pair) %) lst-maps))

(defn write-file [subs]
    (.write @file (reduce #(str %1 %2 "\n" ) "" subs)))

(defn count-subs [set]
  (loop [acc 1 n (count set)]
    (cond
      (> n 3) (recur (* acc (dec n)) (- n 2))
      (= n 2) acc
      :else (* acc 2))))

(defn get-subs [set]
  (let [size (count set)]
    (cond
      (> size 3) (reduce
                  #(let [subs (make-subs %2 (get-subs (without set %2)))]
                     (if (= (count (first subs)) @cardinality)
                                        (write-file subs)
                       (concat %1 subs)))
                  nil (get-pairs set))
      (= size 2) (get-lst-subs2 set)
      :else (get-lst-subs3 set))))

(defn get-subs-size [set]
  (with-open [w (io/writer  "1.4.txt")]
    (reset! cardinality (count set))
    (reset! file w)
    (if (> @cardinality 3)
      (get-subs set)
      (write-file (cond (= @cardinality 3) (get-lst-subs3 set)
                        (= @cardinality 2) (get-lst-subs2 set)
                        :else nil)))
    (.write @file (str "Count of substitutions equals " (count-subs set)))))

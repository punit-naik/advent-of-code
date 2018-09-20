(ns advent-of-code.2017.day-1
  (:require [clojure.string :refer [trim]]))

(defn parse-input
  "Parses the Day 1 input into a seq of numbers"
  []
  (->> (slurp "resources/day-1-input")
       trim
       seq
       (map #(Integer/parseInt (str %)))))

(defn calculate-next-idx
  "Calculates the next index by summing the current index with an offset
   in a circular manner"
  [total-elems current-idx offset]
  (let [sum-by-offset (+ current-idx offset)]
    (if (> sum-by-offset (dec total-elems))
      (- sum-by-offset total-elems) sum-by-offset)))

(defn- form-pairs
  "Maps through an indexed number sequence and
   forms a pair of elements with the first element of the pair being the
   element at the current index position and then next one is calculated by
   offsetting the current position by some value"
  [num-seq offset]
  (map-indexed
    (fn [idx itm]
      (list itm (nth num-seq (calculate-next-idx (count num-seq) idx offset))))
    num-seq))

(defn- reduce-pairs
  "Reduces pairs from a list by comparing them
   One number is taken from every pair where both the elements are equal
   And the result is then sum'ed to get the final result"
  [paired-num-seq offset]
  (->> paired-num-seq
       (map
         (fn [[x y]] (if (= x y) x 0)))
       (reduce +)))

(defn get-captcha
  "Solves for Captcha"
  [input-num-seq offset]
  (-> input-num-seq
      (form-pairs offset)
      (reduce-pairs offset)))

(comment
  ;; Part 1
  (get-captcha (parse-input) 1)
  ;; Part 2
  (get-captcha (parse-input) (/ (count (parse-input)) 2)))

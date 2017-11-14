(ns advent-of-code.core
  (:require [clojure.string :refer [split]]
            [clojure.java.io :refer [file resource]]))

(defn press-keypad
  "Presses the keypad based upon the direction provided"
  [[current-x current-y] direction]
  (condp = direction
    \R [current-x (cond
                    (or (zero? current-x) (= current-x 4)) current-y
                    (or (= current-x 1) (= current-x 3)) (if (< current-y 3) (inc current-y) current-y)
                    (= current-x 2) (if (< current-y 4) (inc current-y) current-y))]
    \L [current-x (cond
                    (or (zero? current-x) (= current-x 4)) current-y
                    (or (= current-x 1) (= current-x 3)) (if (> current-y 1) (dec current-y) current-y)
                    (= current-x 2) (if (> current-y 0) (dec current-y) current-y))]
    \D [(cond
          (or (zero? current-y) (= current-y 4)) current-x
          (or (= current-y 1) (= current-y 3)) (if (< current-x 3) (inc current-x) current-x)
          (= current-y 2) (if (< current-x 4) (inc current-x) current-x)) current-y]
    \U [(cond
          (or (zero? current-y) (= current-y 4)) current-x
          (or (= current-y 1) (= current-y 3)) (if (> current-x 1) (dec current-x) current-x)
          (= current-y 2) (if (> current-x 0) (dec current-x) current-x)) current-y]))
    
(defn split-direction
  "Splits directions from a string"
  [s]
  (seq s))

(defn read-instructions
  "Reads instructions from file"  
  [instructions-file]
  (->> (split (slurp instructions-file) #"\s")
       (mapv split-direction)))

(defn points->keypad-numbers
  "Converts 2-D points into keypad numbers (between 1 and 9)"
  [p]
  (condp = p
    [0 2] "1"
    [1 1] "2"
    [1 2] "3"
    [1 3] "4"
    [2 0] "5"
    [2 1] "6"
    [2 2] "7"
    [2 3] "8"
    [2 4] "9"
    [3 1] "A"
    [3 2] "B"
    [3 3] "C"
    [4 2] "D"))

(defn deduce-code
  "Deduces code using instructions"
  [instructions location-tracker]
  (let [code-tracker (atom [])]
    (doseq [i instructions]
      (doseq [direction i]
        (reset! location-tracker (press-keypad @location-tracker direction)))
      (swap! code-tracker conj @location-tracker))
    ; Converting points to numbers on keypad
    (->> (mapv points->keypad-numbers @code-tracker)
         (reduce str))))

(defn -main
  []
  (->> (deduce-code
         (read-instructions (file (resource "input.txt"))) ; Instructions
         (atom [1 1])) ; Location Tracker
       prn)) 


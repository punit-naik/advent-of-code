(ns advent-of-code.core
  (:require [clojure.string :refer [split]]
            [clojure.java.io :refer [file resource]]))

(defn press-keypad
  "Presses the keypad based upon the direction provided"
  [[current-x current-y] direction]
  (condp = direction
    \R [current-x (if (< current-y 2) (inc current-y) current-y)]
    \L [current-x (if (> current-y 0) (dec current-y) current-y)]
    \D [(if (< current-x 2) (inc current-x) current-x) current-y]
    \U [(if (> current-x 0) (dec current-x) current-x) current-y]))
    
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
    [0 0] "1"
    [0 1] "2"
    [0 2] "3"
    [1 0] "4"
    [1 1] "5"
    [1 2] "6"
    [2 0] "7"
    [2 1] "8"
    [2 2] "9"))

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


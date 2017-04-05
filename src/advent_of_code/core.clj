(ns advent-of-code.core
  (:require [clojure.string :refer [split]]
            [clojure.java.io :refer [file resource]]))

(defn move-on-the-2d-plane
  "Moves the Easter Bunny on the 2-D plane by some distance by doing necessary calculations"
  [[current-orientation current-x current-y] direction distance]
  (cond
    ; Movement in the East direction: +X,Y
    (or (and (= current-orientation "N") (= direction "R")) 
        (and (= current-orientation "S") (= direction "L")))
      ["E" (+ current-x distance) current-y]
    ; Movement in the West direction: -X,Y
    (or (and (= current-orientation "N") (= direction "L"))
        (and (= current-orientation "S") (= direction "R")))
      ["W" (- current-x distance) current-y]
    ; Movement in the South direction: X,-Y
    (or (and (= current-orientation "E") (= direction "R"))
        (and (= current-orientation "W") (= direction "L")))
      ["S" current-x (- current-y distance)]
    ; Movement in the North direction: X,+Y
    (or (and (= current-orientation "E") (= direction "L"))
        (and (= current-orientation "W") (= direction "R")))
      ["N" current-x (+ current-y distance)]
    ; Other cases for no direction; Just moving forward in the current orientation
    (and (= direction "") (= current-orientation "E"))
      [current-orientation (+ current-x distance) current-y]
    (and (= direction "") (= current-orientation "W"))
      [current-orientation (- current-x distance) current-y]
    (and (= direction "") (= current-orientation "S"))
      [current-orientation current-x (- current-y distance)]
    (and (= direction "") (= current-orientation "N"))
      [current-orientation current-x (+ current-y distance)]))
    
(defn split-direction-and-distance
  "Splits direction and distance from a string by applying regex"
  [s]
  (let [[[original-string direction distance]] (re-seq #"(R|L)(\d+)" s)]
    (->> (take (dec (Integer/parseInt distance)) (repeat 1)) ; Moving point by point
         (map (fn [x] ["" x]))
         (cons [direction 1])))) ; Prepending the original direction

(defn read-instructions
  "Reads instructions from file"  
  [instructions-file]
  (->> (split (slurp instructions-file) #",\s")
       (mapv split-direction-and-distance)
       (reduce concat)))
      
(defn calculate-distance
  "Calculates distance of the Easter Bunny from the headquarters"
  [instructions [orientation x y :as orientation-location] location-tracker]
  (if-let [[dir dist] (first instructions)]
    ; Recursively iterate
    (let [[new-orientation new-x new-y :as new-orient-loc] (move-on-the-2d-plane orientation-location dir dist)]
      (if (@location-tracker [new-x new-y])
        ; If already visited, return distance
        (+ (Math/abs new-x) (Math/abs new-y))
        ; Else update location tracker and continue the recursion
        (do
          (swap! location-tracker assoc [new-x new-y] 0)
          (calculate-distance (drop 1 instructions) new-orient-loc location-tracker))))
    ; Return the distance
    (+ (Math/abs x) (Math/abs y))))

(defn -main
  []
  (->> (calculate-distance 
         (read-instructions (file (resource "input.txt"))) ; Instructions
         ["N" 0 0] ; Initial Orientation and Location
         (atom {})) ; Initial Location Tracker
       prn)) 


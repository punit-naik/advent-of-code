(ns advent-of-code.2017.day-4
  (:require [clojure.string :refer [split]]))

(defn read-passphrases
  "Parses the Day 4 input into a list of passphrases"
  []
  (map
    #(split % #"\s+")
    (split (slurp "resources/day-4-input") #"\n")))

(defn- fn-part-1
  "Map fn for part 1"
  [passphrase-list]
  (if (not= (count passphrase-list) (count (distinct passphrase-list))) 0 1))

(defn- fn-part-2
  "Map fn for part 2"
  [passphrase-list]
  (map sort passphrase-list))

(defn count-valid-passphrases
  "Counting all the passphrases which don't contain duplicate words"
  ([passphrase-lists] (count-valid-passphrases passphrase-lists 1))
  ([passphrase-lists part?]
   (->> (if (= part? 1)
          passphrase-lists
          (map fn-part-2 passphrase-lists))
        (map fn-part-1)
        (reduce +))))

(comment
  ;; Part 1
  (count-valid-passphrases (read-passphrases))
  ;; Part 2
  (count-valid-passphrases (read-passphrases) 2))

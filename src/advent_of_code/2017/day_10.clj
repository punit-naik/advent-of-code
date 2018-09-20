(ns advent-of-code.2017.day-10
  (:require [advent-of-code.2017.day-1 :refer [calculate-next-idx]]
            [clojure.string :refer [trim split join]]))

(defn read-input
  [file-name]
  (trim (slurp file-name)))

(defn read-lengths
  "Parses the Day 10 input to lengths"
  [file-name]
  (map
    #(Integer/parseInt %)
    (split
      (read-input file-name) #",")))

(defn chars->ascii
  "Reads the input as characters and then convert them into their corresponding ascii values
   Adds standard suffix values as well"
  [lengths-str]
  (concat
    (->> lengths-str
         (map int))
    '(17 31 73 47 23)))

(defn get-idx-list
  "Gets all the indices of a circular list between two points (indices)"
  [total-elems current-idx offset]
  (take (mod offset total-elems) (drop current-idx (cycle (range total-elems)))))

(defn- reverse-vec
  "Reverse a vector (recursively) between two indices
   in a circular manner"
  [idx-list input-vec]
  (let [start (first idx-list)
        end (last idx-list)]
    (if (= start end)
      input-vec
      (if (> start end) ; circular reverse
        (let [p1 (vec (nthrest (map-indexed #(assoc {} :i %1 :v %2) input-vec) start))
              p2 (take (inc end) (map-indexed #(assoc {} :i %1 :v %2) input-vec))
              merged (map (fn [v1 v2] {:i (:i v1) :v (:v v2)}) (into p1 p2) (reverse (into p1 p2)))]
          (reduce
            (fn [acc {:keys [i v]}] (assoc acc i v))
            input-vec
            merged))
        ; normal subvec reverse
        (into
          (into
            (vec (take start input-vec))
            (reverse (subvec input-vec start (inc end))))
          (nthrest input-vec (inc end)))))))

(defn- tie-knot
  "Manipulates (recursively) the input list to mimic the knot tieng process
   defined in the problem statement"
  [{:keys [input-list lengths prev-idx prev-skip-size] :or {prev-idx 0 prev-skip-size 0}}]
  (let [total-elems (count input-list)]
    (loop [idx prev-idx
           skip-size prev-skip-size
           lens lengths
           modified-list input-list]
      (if (empty? lens)
        modified-list
        (recur
          (calculate-next-idx total-elems idx (+ skip-size (first lens)))
          (inc skip-size)
          (rest lens)
          (if (zero? (first lens))
            modified-list
            (reverse-vec
              (get-idx-list total-elems idx (first lens))
              modified-list)))))))

(defn- tie-knot-n
  "Runs `tie-knot` `n` times"
  [{:keys [input-list lengths]} n]
  (tie-knot {:input-list input-list :lengths (apply concat (repeat n lengths))}))

(defn- dense-hash
  "Dense Hashes the 64 times knot tied list"
  [knot-tied-list]
  (join
    (map
      (fn [s] (->> (apply bit-xor s) (format "%02x")))
      (partition 16 knot-tied-list))))

(defn calculate-hash
  "Calculates the hash from the knot tied list
   by multiplying it's first two elements"
  [input-list lengths]
  (let [modified-list (tie-knot {:input-list (vec input-list) :lengths lengths})]
    (* (first modified-list) (second modified-list))))

(defn calculate-hexadecimal-hash
  "Calculates the final hexadecimal hash of the dense hash of the knot tied list"
  [input-list lengths]
  (dense-hash
    (tie-knot-n {:input-list (vec input-list) :lengths lengths} 64)))

(comment
  ;; Part 1
  (calculate-hash (range 256) (read-lengths "resources/day-10-input"))
  ;; Part 2
  (calculate-hexadecimal-hash (range 256) (chars->ascii (read-input "resources/day-10-input"))))

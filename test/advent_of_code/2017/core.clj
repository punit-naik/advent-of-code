(ns advent-of-code.2017.core
  (:require [clojure.test :refer [deftest testing is]]
            [advent-of-code.2017.day-1 :refer [get-captcha parse-input]]
            [advent-of-code.2017.day-4 :refer [count-valid-passphrases read-passphrases]]
            [advent-of-code.2017.day-10 :refer [calculate-hash read-lengths
                                                calculate-hexadecimal-hash read-input chars->ascii]]))

(deftest day-1-part-1
  (testing "Day 1 Part 1 ..."
    (is (= (get-captcha '(1 1 2 2) 1) 3))
    (is (= (get-captcha '(1 1 1 1) 1) 4))
    (is (= (get-captcha '(1 2 3 4) 1) 0))
    (is (= (get-captcha '(9 1 2 1 2 1 2 9) 1) 9))
    (is (= (get-captcha (parse-input) 1) 1393))))

(deftest day-1-part-2
  (testing "Day 1 Part 2 ..."
    (is (= (get-captcha '(1 2 1 2) 2) 6))
    (is (= (get-captcha '(1 2 2 1) 2) 0))
    (is (= (get-captcha '(1 2 3 4 2 5) 3) 4))
    (is (= (get-captcha '(1 2 3 1 2 3) 3) 12))
    (is (= (get-captcha '(1 2 1 3 1 4 1 5) 4) 4))
    (is (= (get-captcha
             (parse-input)
             (/ (count (parse-input)) 2)) 1292))))

(deftest day-4-part-1
  (testing "Day 4 Part 1 ..."
    (is (= (count-valid-passphrases
             [["aa" "bb" "cc" "dd" "ee"]
              ["aa" "bb" "cc" "dd" "aa"]
              ["aa" "bb" "cc" "dd" "aaa"]]) 2))
    (is (= (count-valid-passphrases (read-passphrases)) 386))))

(deftest day-4-part-2
  (testing "Day 4 Part 2 ..."
    (is (= (count-valid-passphrases
             [["abcde" "fghij"]
              ["abcde" "xyz" "ecdab"]
              ["a" "ab" "abc" "abd" "abf" "abj"]
              ["iiii" "oiii" "ooii" "oooi" "oooo"]
              ["oiii" "ioii" "iioi" "iiio"]] 2) 3))
    (is (= (count-valid-passphrases (read-passphrases) 2) 208))))

(deftest day-10-part-1
  (testing "Day 10 Part 1..."
    (is (= (calculate-hash [0 1 2 3 4] [3 4 1 5]) 12))
    (is (= (calculate-hash (range 256) (read-lengths "resources/day-10-input")) 5577))))

(deftest day-10-part-2
  (testing "Day 10 Part 2..."
    (is (= (calculate-hexadecimal-hash (range 256) (chars->ascii "")) "a2582a3a0e66e6e86e3812dcb672a272"))
    (is (= (calculate-hexadecimal-hash (range 256) (chars->ascii "AoC 2017")) "33efeb34ea91902bb2f59c9920caa6cd"))
    (is (= (calculate-hexadecimal-hash (range 256) (chars->ascii "1,2,3")) "3efbe78a8d82f29979031a4aa0b16a9d"))
    (is (= (calculate-hexadecimal-hash (range 256) (chars->ascii "1,2,4")) "63960835bcdc130f0b66d7ff4f6a5a8e"))
    (is (= (calculate-hexadecimal-hash
            (range 256)
            (chars->ascii (read-input "resources/day-10-input")))
           "44f4befb0f303c0bafd085f97741d51d"))))

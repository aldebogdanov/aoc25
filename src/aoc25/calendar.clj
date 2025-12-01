(ns aoc25.calendar
  (:require [hyperfiddle.rcf :as rcf :refer [tests]]
            [clojure.java.io :as io]
            [aoc25.day1 :as day1]))

(rcf/enable!)

(tests
 
 "Day 1" 
 (def input (slurp (io/resource "day1.input")))

 (day1/puzzle1-a input) := 1182      ; 1.243256 ms
 (day1/puzzle1-b input) := 1182      ; 662.093426 µs
 
 (day1/puzzle2-a input) := 6907      ; 34.688584 ms
 (day1/puzzle2-b input) := 6907      ; 19.473641 ms
 (day1/puzzle2-c input) := 6907      ; 58.884749 ms
 (day1/puzzle2-d input) := 6907      ; 15.054112 ms
 )

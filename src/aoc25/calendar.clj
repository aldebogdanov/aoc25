(ns aoc25.calendar
  (:require [hyperfiddle.rcf :as rcf :refer [tests]]
            [clojure.java.io :as io]
            [aoc25.day1 :as day1]
            [aoc25.day2 :as day2]))

(rcf/enable!)

(tests

 "Day One"
 (def input (slurp (io/resource "day1.input")))

 (day1/puzzle-1-a input) := 1182      ; 1.243256 ms
 (day1/puzzle-1-b input) := 1182      ; 662.093426 µs

 (day1/puzzle-2-a input) := 6907      ; 34.688584 ms
 (day1/puzzle-2-b input) := 6907      ; 19.473641 ms
 (day1/puzzle-2-c input) := 6907      ; 58.884749 ms
 (day1/puzzle-2-d input) := 6907      ; 15.054112 ms

 "Day Two"
 (def input (slurp (io/resource "day2.input")))

 (day2/puzzle-1-a input) := 19605500130      ; 148.350040 ms
 (day2/puzzle-1-b input) := 19605500130      ; 348.956374 ms

 (day2/puzzle-2-a input) := 36862281418      ; 358.993867 ms
 (day2/puzzle-2-b input) := 36862281418      ; 511.376895 ms
 )

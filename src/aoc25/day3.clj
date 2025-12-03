(ns aoc25.day3
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [hyperfiddle.rcf :refer [tests]] 
            [criterium.core :as crit]))

(def example "987654321111111\n811111111111119\n234234234234278\n818181911112111")

(defn- solve-1-a
  [line]
  (let [m (apply max line)
        idxs (keep-indexed #(when (= %2 m) %1) line)]
    (if (< 1 (count idxs))
      (+ (* 10 m) m)
      (let [i (first idxs)]
        (if (= i (-> line count dec))
          (+ (* 10 (apply max (drop-last line))) m)
          (+ (* 10 m) (apply max (drop (inc i) line))))))))


(defn solve-2-a
  [line]
  (loop [i 11
         ns line
         x 0]
    (if (zero? i)
      (+ x (apply max ns))
      (let [ns' (drop-last i ns)
            m (apply max ns')
            idx (first (keep-indexed #(when (= %2 m) %1) ns'))]
        (recur (dec i)
               (drop (inc idx) ns)
               (+ x (* (long (Math/pow 10 i)) m)))))))


(defn- puzzle-x
  [solve-fn input]
  (->> input
       (str/split-lines)
       (map #(mapv (comp Long/parseLong str) %))
       (map solve-fn)
       (reduce + 0)))


(defn puzzle-1-a
  [input]
  (puzzle-x solve-1-a input))


(defn puzzle-2-a
  [input]
  (puzzle-x solve-2-a input))


(tests
 (puzzle-1-a example) := 357
 (puzzle-2-a example) := 3121910778619)
 

(comment
  (def input (slurp (io/resource "day3.input")))

  (crit/quick-bench (puzzle-1-a input))      ; 1.118041 ms   
  (crit/quick-bench (puzzle-2-a input))      ; 6.047108 ms
  )
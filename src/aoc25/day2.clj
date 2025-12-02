(ns aoc25.day2
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [hyperfiddle.rcf :refer [tests]] 
            [criterium.core :as crit]))

(def example "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")


(defn- check-same-digits-subs?
  [n]
  (let [s (str n)]
    (when (even? (count s))
      (let [c2 (/ (count s) 2)]
        (= (subs s 0 c2) (subs s c2))))))


(defn- puzzle-x-fn
  [pred input]
  ((fn [in]
      (->> (into '()
                 (comp
                  (mapcat (fn [range-str]
                            (let [[a b] (map Long/parseLong (str/split range-str #"-"))]
                              (concat (range a b) (list b)))))
                  (filter pred))
                 (str/split in #","))
           (reduce + 0))) input))


(defn puzzle-1-a
  [input]
  (puzzle-x-fn check-same-digits-subs? input))


(defn puzzle-1-b
  [input]
  (puzzle-x-fn #(re-matches #"(.+?)\1" (str %)) input))


(defn puzzle-2-a
  [input]
  (puzzle-x-fn #(re-matches #"(.+?)\1{1,}" (str %)) input))


(defn- check-same-seqs
  [n]
  (let [s (str n)
        c-max (long (/ (count s) 2))]
    (some #(let [c (inc %)
                 sub (subs s 0 c)]
             (loop [ss (subs s c)]
               (cond
                 (nil? (seq ss)) true
                 (str/starts-with? ss sub) (recur (subs ss c))
                 :else false)))
          (range c-max))))


(defn puzzle-2-b
  [input]
  (puzzle-x-fn check-same-seqs input))



(tests
 (puzzle-1-a example) := 1227775554
 (puzzle-1-b example) := 1227775554
 (puzzle-2-a example) := 4174379265
 (puzzle-2-b example) := 4174379265)
 

(comment
  (def input (slurp (io/resource "day2.input")))

  (crit/quick-bench (puzzle-1-a input))   ;; 148.350040 ms
  (crit/quick-bench (puzzle-1-b input))   ;; 348.956374 ms
  (crit/quick-bench (puzzle-2-a input))   ;; 358.993867 ms
  (crit/quick-bench (puzzle-2-b input))   ;; 511.376895 ms
  

  ;; === FUN ===

  (defn check-same-digits-log10?
    [n]
    (let [c (-> n Math/log10 long inc)]
      (when (even? c)
        (let [d (long (Math/pow 10 (/ c 2)))
              a (Math/divideExact n d)
              b (- n (* d a))]
          (= a b)))))

  (defn check-same-digits-drop?
    [n]
    (let [s (str n)]
      (when (even? (count s))
        (let [c2 (/ (count s) 2)]
          (= (drop c2 s) (drop-last c2 s))))))
  
  (defn check-same-digits-regex?
    [n]
    (re-matches #"(.+?)\1" (str n)))

  (def r (range 1 100000))

  (crit/quick-bench (count (filter check-same-digits-log10? r)))    ; 13.355160 ms
  (crit/quick-bench (count (filter check-same-digits-subs? r)))     ; 2.564123 ms
  (crit/quick-bench (count (filter check-same-digits-drop? r)))     ; 3.703529 ms
  (crit/quick-bench (count (filter check-same-digits-regex? r)))    ; 9.870951 ms
  )
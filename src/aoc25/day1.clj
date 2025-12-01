(ns aoc25.day1
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [hyperfiddle.rcf :refer [tests]] 
            [criterium.core :as crit]))

(defn puzzle1-a
  [input]
  (loop [counter 0
         value   50
         in      (str/split-lines input)]
    (let [[_ a b] (re-matches #"([LR])(\d+)" (first in))
          n (cond-> (Long/parseLong b)
              (= a "L") Math/negateExact)
          x (mod (+ 100 value n) 100)
          vs (rest in)]
      (if (seq vs)
        (recur ((case x 0 inc identity) counter) x vs)
        counter))))


(defn puzzle2-a
  [input]
  (loop [moves (str/split-lines input)
         position 50
         clicks 0]
    (let [move (first moves)]
      (cond
        (nil? move) clicks
        (string? move) (recur (-> move (subs 1) (Long/parseLong) (repeat (first move)) (concat (rest moves))) position clicks)
        :else (let [x (if (= \L move) (dec position) (inc position))
                    y (cond
                        (= -1 x) 99
                        (= 100 x) 0
                        :else x)]
                (recur (rest moves) y (if (zero? y) (inc clicks) clicks)))))))

;; == MORE IDEAS

(defn puzzle2-b
  [input]
  (let [position (atom 50)
        count (atom 0)]
    (add-watch position :zero-clicks #(when (zero? (mod %4 100)) (swap! count inc)))
    (loop [moves (str/split-lines input)]
      (if-let [move (first moves)]
        (let [f (case (first move) \L dec inc)]
          (dotimes [_ (Long/parseLong (subs move 1))] (swap! position f))
          (recur (rest moves)))
        @count))))


(defn puzzle1-b
  [input]
  (second (reduce (fn [[acc cnt] move]
                    (let [a ((case (first move) \L - +) (first acc) (Long/parseLong (subs move 1)))]
                      [(conj acc a) (if (zero? (mod a 100)) (inc cnt) cnt)]))
                  ['(50) 0]
                  (str/split-lines input))))


(defn puzzle2-c
  [input]
  (let [rf (fn 
             [acc move]
             (let [a (first acc)
                   f (case (first move) \L - +)
                   b (f a (Long/parseLong (subs move 1)))]
               (conj (into acc (drop 1 (range a b (f 1)))) b)))]
  (->> input
       str/split-lines
       (reduce rf '(50))
       (filter #(zero? (mod % 100)))
       count)))


(defn puzzle2-d
  [input]
  (count (into []
               (comp
                (mapcat (let [a (atom 50)]
                          (fn [i]
                            (let [[k n] [(first i) (Long/parseLong (subs i 1))]
                                  f (case k \L - +)
                                  b (f @a n)
                                  r (conj (into '() (drop 1 (range @a b (f 1)))) b)]
                              (reset! a b)
                              r))))
                (filter #(zero? (mod % 100))))
               (str/split-lines input))))


(tests
 (puzzle1-a "L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82") := 3
 (puzzle1-b "L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82") := 3

 (puzzle2-a "L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82") := 6
 (puzzle2-b "L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82") := 6
 (puzzle2-c "L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82") := 6
 (puzzle2-d "L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82") := 6)


(comment
  (def input (slurp (io/resource "day1.input")))

  (crit/quick-bench (puzzle1-a input))
  (crit/quick-bench (puzzle1-b input))
  
  (crit/quick-bench (puzzle2-a input))
  (crit/quick-bench (puzzle2-b input))
  (crit/quick-bench (puzzle2-c input))
  (crit/quick-bench (puzzle2-d input))
  )
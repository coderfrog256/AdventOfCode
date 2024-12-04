(ns zone.frog (:require [clojure.string :as str] [clojure.math :as math]))

(defn read-input [filename]
  (let [input (str/split-lines (slurp filename))]
    (into {} (for [row (range (count input))
                   [col char] (map-indexed vector (str/split (nth input row) #""))]
               [[row col] char]))))

(defn part-1 [file]
  (let [to-check #{"XMAS" "SAMX"}
        input (read-input file)
        length (reduce max (map first (keys input)))
        string-builder (fn [func] (if (contains? to-check (apply str [(func 0) (func 1) (func 2) (func 3)])) 1 0))]
    (reduce (fn [matches row]
              (reduce (fn [matches col]
                        (reduce + matches
                                (map string-builder
                                     [#(get input [col (+ % row)])
                                      #(get input [(+ col %) row])
                                      #(get input [(+ col %) (+ row %)])
                                      #(get input [(+ col %) (- row %)])])))
                      matches (range (inc length))))
            0 (range (inc length)))))
(println (time (part-1 "../input/day4-test.txt")))
(println (time (part-1 "../input/day4.txt")))

(defn part-2 [file]
  (println)
  (let [to-check #{"M" "S"}
        input (read-input file)
        length (reduce max (map first (keys input)))]
    (reduce (fn [matches row]
              (+ matches (reduce (fn [matches col]
                                   (+ matches
                                      (let [upper-left (get input [col row]) upper-right (get input [col (+ 2 row)])
                                            mid (get input [(+ 1 col) (+ 1 row)])
                                            lower-left (get input [(+ 2 col) row]) lower-right (get input [(+ 2 col) (+ 2 row)])]
                                        (if (and (contains? to-check upper-left) (contains? to-check upper-right)
                                                 (contains? to-check lower-left) (contains? to-check lower-right)
                                                 (not= upper-left lower-right) (not= upper-right lower-left)
                                                 (= mid "A"))
                                          1 0))))
                                 0 (range (inc length)))))
            0 (range (inc length)))))
(print (time (part-2 "../input/day4-test.txt")))
(print (time (part-2 "../input/day4.txt")))

(ns zone.frog (:require [clojure.string :as str] [clojure.math :as math]))

(defn run [file part-1]
  (let [parts (str/split file #"\n\n")
        groups (group-by first (map #(map parse-long (str/split % #"\|")) (str/split (first parts) #"\n")))
        pages (zipmap (keys groups) (->> (vals groups) (map #(map second %)) (map #(into #{} %))))]
    (->> (str/split (second parts) #"\n")
         (map (fn [line]
                (let [sorted (->> (str/split line #",")
                                  (map parse-long)
                                  (sort #(cond (contains? (get pages %1) %2) -1
                                               (contains? (get pages %2) %1) 1)))]
                  (if ((if part-1 = not=) line (str/join "," sorted))
                    (nth sorted (math/floor (/ (count sorted) 2))) 0))))
         (reduce +))))

(println (time (run (get-advent-of-code-input "../input/day5-test.txt") true)))
(println (time (run (get-advent-of-code-input "../input/day5.txt") true)))
(println (time (run (get-advent-of-code-input "../input/day5-test.txt") false)))
(println (time (run (get-advent-of-code-input "../input/day5.txt") false)))

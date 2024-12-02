(ns zone.frog (:require [clojure.string :as str]
                        [clojure.math :as math]))
(defn is-safe [nums]
  (some? (reduce (fn [direction [left right]]
                   (let [diff (- left right)
                         signum (math/signum diff)]
                     (if (or (and (some? direction) (not= direction signum))
                             (> (Math/abs diff) 3))
                       (reduced nil) signum)))
                 nil (partition 2 1 nums))))

(defn part-one [file]
  (->> (str/split-lines (slurp file))
       (map #(->> (str/split % #"\s+") (map read-string)))
       (filter is-safe)
       (count)))

(println (time (part-one "../input/day2-test.txt")))
(println (time (part-one "../input/day2.txt")))

(defn part-two [file]
  (->> (str/split-lines (slurp file))
       (map #(->> (str/split % #"\s+") (mapv read-string)))
       (map (fn [l] (map #(vec (concat (subvec l 0 %) (subvec l (inc %))))
                         (range 0 (count l)))))
       (filter #(some is-safe %))
       (count)))

(println (time (part-two "../input/day2-test.txt")))
(println (time (part-two "../input/day2.txt")))

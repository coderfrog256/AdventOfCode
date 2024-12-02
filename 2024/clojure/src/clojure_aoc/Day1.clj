(ns frog.zone)

(defn zip-lists [lists] (apply map vector lists))

(defn part-one [file]
  (->> (slurp file)
       (clojure.string/split-lines)
       (map #(->> (clojure.string/split % #"\s+")
                  (map read-string)
                  (vec)))
       (zip-lists)
       (map sort)
       (zip-lists)
       (map (fn [[a b]] (abs (- a b))))
       (reduce +)))

(part-one "../input/day1.txt")

(defn part-two [file]
  (let [[left right] (->> (slurp file)
                          (clojure.string/split-lines)
                          (map #(->> (clojure.string/split % #"\s+")
                                     (map read-string)
                                     (vec)))
                          (zip-lists))
        right-count (frequencies right)]
    (reduce + (map (fn [l] (* l (get right-count l 0))) left))))

(part-two "../input/day1.txt")

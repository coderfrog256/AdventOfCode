(ns zone.frog (:require [clojure.string :as str] [clojure.math :as math]))

;; Cache of file contents
(def input-cache (atom {}))

;; Method to read file contents using the cache
(defn get-advent-of-code-input [file]
  (if-let [content (get @input-cache file)]
    content
    (let [content (slurp file)]
      (swap! input-cache assoc file content)
      content)))

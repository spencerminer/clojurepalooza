(ns advent-of-code-2021.util
  (:require [clojure.java.io :as io]))

(defn get-lines [day]
  (->> day
       (format "advent-of-code-2021/%s.txt")
       io/resource
       io/reader
       line-seq))

(defn prn-rtn [arg]
  (clojure.pprint/pprint arg)
  arg)

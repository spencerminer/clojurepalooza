(ns clojurepalooza.sort-imports
  (:import (java.io PushbackReader))
  (:require [clojure.string :as string]
            [clojure.pprint :as pprint]
            [clojure.edn :as edn]))

;; Comment ????
(comment foo Comment???)

(def args *command-line-args*)

(with-open [in (PushbackReader. (clojure.java.io/reader (first args)))]
  (let [edn-seq (repeatedly (partial edn/read {:eof :theend} in))]
    (doseq [form (take-while (partial not= :theend) edn-seq)]
      (pprint/pprint
       (if (= (first form) 'ns)
         (concat (butlast form)
                 (list (cons :require (sort (rest (last form))))))
         form))
      (println))))
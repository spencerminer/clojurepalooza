(ns clojurepalooza.clipboard
  (:require [fipp.edn :as fipp])
  (:import (java.awt.datatransfer DataFlavor Transferable StringSelection)
           (java.awt Toolkit)
           (java.io StringWriter)))

;; From baskeboler:
;; https://gist.github.com/baskeboler/7d226374582246d28b25801e28e18216

(defn get-clipboard
  "get system clipboard"
  []
  (-> (Toolkit/getDefaultToolkit)
      (.getSystemClipboard)))

(defn slurp-clipboard
  "get latest string from clipboard"
  []
  (when-let [^Transferable clip-text (some-> (get-clipboard)
                                             (.getContents nil))]
    (when (.isDataFlavorSupported clip-text DataFlavor/stringFlavor)
      (->> clip-text
           (#(.getTransferData % DataFlavor/stringFlavor))
           (cast String)))))

(defn spit-clipboard
  "write string s to clipboard"
  [s]
  (let [sel (StringSelection. s)]
    (some-> (get-clipboard)
            (.setContents sel sel))))

(defn with-out-clipboard
  "pretty prints a data structure into the clipboard using fipp library"
  [d]
  (let [wr (StringWriter.)]
    (fipp/pprint d {:writer wr})
    (spit-clipboard (.toString wr))))
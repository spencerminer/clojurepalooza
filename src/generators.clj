(ns generators
  (:require [clojure.string :as string]
            [clojure.test :refer [is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop])
  (:import (java.net URL URLEncoder)))


;; scheme:[//[user:password@]host[:port]][/]path[?query][#fragment]


(def gen-domain-name
  (gen/let [parts (gen/vector
                   (gen/not-empty gen/string-alpha-numeric)
                   1
                   5)]
    (string/join "." parts)))

(def gen-ipv4-address
  (gen/fmap (partial string/join ".")
            (gen/vector (gen/choose 0 255) 4)))

(def gen-host
  (gen/one-of [gen-domain-name
               gen-ipv4-address]))

(defn url-encode [s]
  (URLEncoder/encode s))

(def gen-path
  (gen/let [parts (gen/vector
                   (gen/fmap url-encode gen/string)
                   0
                   4)]
    (string/join "/" parts)))

(def gen-url
  (gen/let [scheme (gen/elements ["http" "https" "ftp"])
            host   gen-host
            path   (gen/one-of [(gen/return nil)
                                gen-path])]
    (format "%s://%s%s"
            scheme
            host
            (if (nil? path) "" (str "/" path)))))

(defspec good-url? 500
  (prop/for-all [url gen-url]
    (is (URL. url))))

(defspec case-equivalency
  (prop/for-all [s gen/string]
    (is (= (string/lower-case s)
           (string/lower-case (string/upper-case s))))))

(defproject clojurepalooza "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [com.cerner/clara-rules "0.21.0"]
                 [cheshire "5.10.0"]
                 [clj-http "3.10.0"]
                 [clj-time "0.15.2"]
                 [clojure.java-time "0.3.2"]
                 [hiccup "1.0.5"]
                 [hickory "0.7.1"]
                 [org.clojure/data.csv "1.0.0"]
                 [org.clojure/data.generators "1.0.0"]
                 [org.clojure/test.check "0.10.0"]]
  :repl-options {:init-ns clojurepalooza.core})

(ns clojurepalooza.quine
  (:require [clojure.string :as clojure.string]))

(fn quine []
  (let [space (char 32)
        d-quote (char 34)
        strs
        [
         "(fn quine []"
         "(let [space (char 32)"
         "d-quote (char 34)"
         "strs"
         "["
         "]]"
         "(->> strs"
         "(map (fn [line] (str d-quote line d-quote)))"
         "((fn [*] (concat [(clojure.string/join (str space) (take 5 strs))]"
         "[(clojure.string/join (str space) *)]"
         "[(clojure.string/join (str space) (drop 5 strs))])))"
         "clojure.string/join)))"
         ]]
    (->> strs
         (map (fn [line] (str d-quote line d-quote)))
         ((fn [*] (concat [(clojure.string/join (str space) (take 5 strs))]
                          [(clojure.string/join (str space) *)]
                          [(clojure.string/join (str space) (drop 5 strs))])))
         clojure.string/join)))

(fn [] ((fn [x] (str (list (quote fn) [] (list x (list (quote quote) x))))) (quote (fn [x] (str (list (quote fn) [] (list x (list (quote quote) x))))))))

(= (str '(fn quine []
           (let [space (char 32)
                 d-quote (char 34)
                 strs
                 [
                  "(fn quine []"
                  "(let [space (char 32)"
                  "d-quote (char 34)"
                  "strs"
                  "["
                  "]]"
                  "(->> strs"
                  "(map (fn [line] (str d-quote line d-quote)))"
                  "((fn [*] (concat [(clojure.string/join (str space) (take 5 strs))]"
                  "[(clojure.string/join (str space) *)]"
                  "[(clojure.string/join (str space) (drop 5 strs))])))"
                  "clojure.string/join)))"
                  ]]
             (->> strs
                  (map (fn [line] (str d-quote line d-quote)))
                  ((fn [*] (concat [(clojure.string/join (str space) (take 5 strs))]
                                   [(clojure.string/join (str space) *)]
                                   [(clojure.string/join (str space) (drop 5 strs))])))
                  clojure.string/join))))
   ((fn quine []
      (let [space (char 32)
            d-quote (char 34)
            strs
            [
             "(fn quine []"
             "(let [space (char 32)"
             "d-quote (char 34)"
             "strs"
             "["
             "]]"
             "(->> strs"
             "(map (fn [line] (str d-quote line d-quote)))"
             "((fn [*] (concat [(clojure.string/join (str space) (take 5 strs))]"
             "[(clojure.string/join (str space) *)]"
             "[(clojure.string/join (str space) (drop 5 strs))])))"
             "clojure.string/join)))"
             ]]
        (->> strs
             (map (fn [line] (str d-quote line d-quote)))
             ((fn [*] (concat [(clojure.string/join (str space) (take 5 strs))]
                              [(clojure.string/join (str space) *)]
                              [(clojure.string/join (str space) (drop 5 strs))])))
             clojure.string/join)))))
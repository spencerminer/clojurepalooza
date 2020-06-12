(ns clojurepalooza.macro-practice)

;(eval (list 'let ['result (+ 1 2)]
;            (list 'println 'result)
;            'result))
;(eval +)
;(eval (let [test 'nil?
;            body '((println "Yo"))]
;        (list 'if test (cons 'do body))))
;(conj '(:c 4) :a)

;(eval `(+ 1 ~(+ 3 4)))

(defmacro code-critic
  "Phrases are courtesy Hermes Conrad from Futurama"
  [bad good]
  (list 'do
        (list 'println
              "Great squid of Madrid, this is bad code:"
              (list 'quote bad))
        (list 'println
              "Sweet gorilla of Manila, this is good code:"
              (list 'quote good))))
(defmacro code-critic2 [bad good]
  `(do
     (println "Great squid of Madrid, this is bad code:" `'~bad)
     (println "Sweet gorilla of Manila, this is good code:" '~good)))

;(code-critic (1 + 1) (+ 1 1))
;(macroexpand '(code-critic2 (1 + 1) (+ 1 1)))

(defmacro my-print
  [expression]
  (list 'let ['result expression]
        (list 'println 'result)
        'result))

(defmacro my-print2 [expression]
  `(let [result ~expression]
     (println result)
     result))

;(my-print (* 3 5))
;(macroexpand-1 '(my-print2 (* 3 5)))
;(macroexpand-1 '(my-print (* 3 5)))

(defmacro my-wand
  [coll]
  `(println ~coll))

;(let [c [(+ 1 2) (+ 2 3)]]
;  (my-wand c))

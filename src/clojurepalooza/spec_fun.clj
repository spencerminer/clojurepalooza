(ns clojurepalooza.spec-fun
  (:require [clojure.spec.alpha :as s]
            [clojure.test.check.generators :as gen]
            [clojure.data.generators :as dgen]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1A
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(s/def ::even-nums (fn [x]
                     (and (even? x)
                          (< 10 x))))
(s/valid? ::even-nums 20)
(gen/sample (s/gen (s/and int? ::even-nums)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1B
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(s/def ::capital-ascii (s/spec (partial re-matches #"[A-Z]{6,10}")
                               :gen #(as-> (gen/choose \A \Z) x
                                           (gen/fmap char x)
                                           (gen/vector x 1 7)
                                           (gen/fmap clojure.string/join x))))

(gen/sample (s/gen ::capital-ascii))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1C
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#_(s/def ::card (set (for [suit #{:heart :spade :club :diamond}
                           face #{:ace :jack :king :queen 2 3 4 5 6 7 8 9}]
                       [suit face])))

(s/def ::suit #{:heart :spade :club :diamond})
(s/def ::face #{:ace :jack :king :queen 2 3 4 5 6 7 8 9})
(s/def ::joker #{:joker})

(gen/sample (s/gen ::suit))
(gen/sample (s/gen ::face))

(s/def ::card (s/or :joker ::joker
                    :regular (s/tuple ::face ::suit)))

(s/valid? ::card :joker)

(gen/sample (s/gen ::card) 100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(gen/sample (gen/such-that (fn [hand]
                             (-> hand distinct count (= 5)))
                           (gen/vector (s/gen ::card) 5)
                           100)
            1)

;; This doesn't work quite yet
(gen/sample (dgen/weighted {gen/small-integer 0.5
                            gen/string 0.5}) 30)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(s/def ::upgrades )
(s/def ::name (s/spec (partial re-matches #"[a-z]{6,10}")
                      :gen #(as-> (gen/choose \a \z) x
                                  (gen/fmap char x)
                                  (gen/vector x 6 10)
                                  (gen/fmap clojure.string/join x))))
(s/def ::gold (s/and int? #(<= 0 %)))
(s/def ::races #{"orchid" "inox" "quatryl" "human" "harrower" "valrast" "aesther" "vermling" "savve"})
(s/def ::classes #{"brute" "cragheart" "mindthief" "scoundrel" "spellweaver" "tinkerer"})
(s/def ::reputation (s/and int? #(< -10 % 10)))
(s/def ::health (s/and int? #(<= 1 % 50)))
(s/def ::experience (s/and int? #(<= 0 %)))
(s/def ::level (s/and int? #(<= 1 % 10)))

(s/def ::gh-character (s/keys :req-un [::name
                                       ::gold
                                       ::races
                                       ::classes
                                       ::reputation
                                       ::health
                                       ::experience]))

(gen/sample
 (gen/let [my-character (s/gen ::gh-character)]
   (assoc my-character
          :level (-> my-character :experience inc (* 10))))
 1)

(gen/sample (s/gen ::gh-character) 1)
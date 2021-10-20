(ns useful-unloved)

(defn nested-map [[k & ks] v]
  (if ks
    {k (nested-map ks v)}
    {k v}))

(defn select-keys* [m paths]
  (apply merge-with merge
         (map (fn [ks]
                (when-let [v (get-in m ks)]
                  (nested-map ks v)))
              paths)))

(comment
 (select-keys* {:a 1
                :b {:c 21
                    :d 22}
                :e {:f 31
                    :g {:h 321}}}
               [[:a]
                [:b :d]
                [:e :g :h]
                [:e :f]])
 )
(ns clojurepalooza.imdb
  (:require [clojure.data.csv :as csv]))

;; Data can be downloaded here: https://www.imdb.com/interfaces/
(def show-id "tt0436992")   ;; Doctor Who

(def episodes-tsv "resources/title.episode.tsv")
(def ratings-tsv "resources/title.ratings.tsv")

(defn str->int [s] (Integer. ^String s))
(defn str->float [s] (Float. ^String s))

(defn map-tsv-file [f]
  (let [[header & rows] (-> f slurp (csv/read-csv :separator \tab))
        header (map keyword header)]
    (map (partial zipmap header) rows)))

(def episodes
  (->> (map-tsv-file episodes-tsv)
       (filter #(= show-id (:parentTconst %)))
       (remove #(= "\\N" (:seasonNumber %)))
       (map #(hash-map (keyword (:tconst %))
                       (-> %
                           (select-keys [:seasonNumber :episodeNumber])
                           (update :seasonNumber str->int)
                           (update :episodeNumber str->int))))
       (sort-by (comp :episodeNumber second))
       (sort-by (comp :seasonNumber second))
       (apply merge)))

(def ratings
  (->> (map-tsv-file ratings-tsv)
       (map #(hash-map (keyword (:tconst %))
                       (-> %
                           (select-keys [:averageRating :numVotes])
                           (update :averageRating str->float)
                           (update :numVotes str->int))))
       (apply merge)))

(def rated-episodes
  (vec (clojure.set/intersection (set (keys episodes))
                                 (set (keys ratings)))))

(def episode-ratings
  (merge-with merge
              (select-keys episodes rated-episodes)
              (select-keys ratings rated-episodes)))

(def top-10
  (->> episode-ratings
       (sort-by (comp - :averageRating second))
       (take 10)))

(def google-sheets-import-format
  (->> episode-ratings
       vals
       (sort-by :episodeNumber)
       (sort-by :seasonNumber)
       (map #(vector (format "s%02de%02d"
                             (:seasonNumber %) (:episodeNumber %))
                     (:averageRating %)))))

(comment
s01e01 7.5
s01e02 7.5
s01e03 7.5
s01e04 7.0
s01e05 7.0
s01e06 8.7
s01e07 7.1
s01e08 8.4
s01e09 9.2
s01e10 9.1
s01e11 7.1
s01e12 8.7
s01e13 9.1
s02e00 8.1
s02e01 7.4
s02e02 7.8
s02e03 8.3
s02e04 9.3
s02e05 7.8
s02e06 7.9
s02e07 6.7
s02e08 8.7
s02e09 8.8
s02e10 6.1
s02e11 5.9
s02e12 8.5
s02e13 9.3
s03e00 7.6
s03e01 8.0
s03e02 7.7
s03e03 8.0
s03e04 7.0
s03e05 6.9
s03e06 6.6
s03e07 7.3
s03e08 9.0
s03e09 9.2
s03e10 9.8
s03e11 8.7
s03e12 8.7
s03e13 8.4
s04e00 7.6
s04e01 7.8
s04e02 8.0
s04e03 8.1
s04e04 7.5
s04e05 7.5
s04e06 7.6
s04e07 7.7
s04e08 9.4
s04e09 9.5
s04e10 9.1
s04e11 8.9
s04e12 9.1
s04e13 9.2
s05e01 8.8
s05e02 7.6
s05e03 6.7
s05e04 8.7
s05e05 8.6
s05e06 7.0
s05e07 8.2
s05e08 7.2
s05e09 7.4
s05e10 9.3
s05e11 8.2
s05e12 9.1
s05e13 9.1
s06e00 8.6
s06e01 8.9
s06e02 8.8
s06e03 6.7
s06e04 9.0
s06e05 7.2
s06e06 7.4
s06e07 9.1
s06e08 8.0
s06e09 7.0
s06e10 8.5
s06e11 8.1
s06e12 7.6
s06e13 8.5
s07e00 7.2
s07e01 8.6
s07e02 7.4
s07e03 7.3
s07e04 7.5
s07e05 9.0
s07e06 8.4
s07e07 7.9
s07e08 7.3
s07e09 7.2
s07e10 7.7
s07e11 7.4
s07e12 7.3
s07e13 7.3
s07e14 9.0
s08e00 8.4
s08e01 7.9
s08e02 7.6
s08e03 7.0
s08e04 8.9
s08e05 7.9
s08e06 7.3
s08e07 6.8
s08e08 8.5
s08e09 8.4
s08e10 6.0
s08e11 8.6
s08e12 7.8
s09e00 8.2
s09e01 8.4
s09e02 8.5
s09e03 8.2
s09e04 8.0
s09e05 7.5
s09e06 7.4
s09e07 7.8
s09e08 8.5
s09e09 5.8
s09e10 8.5
s09e11 9.6
s09e12 8.6
s09e13 8.5
s10e00 7.4
s10e01 7.9
s10e02 7.2
s10e03 7.3
s10e04 7.4
s10e05 8.1
s10e06 8.4
s10e07 7.6
s10e08 7.2
s10e09 7.0
s10e10 6.9
s10e11 9.2
s10e12 9.0
s11e00 8.1
s11e01 6.8
s11e02 5.9
s11e03 6.9
s11e04 4.9
s11e05 4.9
s11e06 6.3
s11e07 6.1
s11e08 5.6
s11e09 6.0
s11e10 5.1
s11e11 5.6
s12e01 6.5
s12e02 6.3
s12e03 4.0
s12e04 6.4
s12e05 7.4
s12e06 5.1
s12e07 5.6
s12e08 7.0
s12e09 6.5
s12e10 5.1
s12e11 6.0
 )




(comment
 ;; Bash for splitting huge csv
 ;; split -l 500000 title.basics-smaller.tsv
 ;; move the split files into a new folder
 ;; for filename in $(ls);
 ;;   do mv $filename
 ;;         $(echo title.basics-split-$filename.tsv);
 ;; done;
 )

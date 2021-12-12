(ns day12
  (:require [clojure.string :as str]
            [medley.core :refer [map-vals]]))

(def example-input "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end")
(def example-input2 "dc-end\nHN-start\nstart-kj\ndc-start\ndc-HN\nLN-dc\nHN-end\nkj-sa\nkj-HN\nkj-dc")
(def example-input3 "fs-end\nhe-DX\nfs-he\nstart-DX\npj-DX\nend-zg\nzg-sl\nzg-pj\npj-he\nRW-he\nfs-DX\npj-RW\nzg-RW\nstart-pj\nhe-WI\nzg-he\npj-fs\nstart-RW")
(def puzzle-input "TR-start\nxx-JT\nxx-TR\nhc-dd\nab-JT\nhc-end\ndd-JT\nab-dd\nTR-ab\nvh-xx\nhc-JT\nTR-vh\nxx-start\nhc-ME\nvh-dd\nJT-bm\nend-ab\ndd-xx\nend-TR\nhc-TR\nstart-vh")

(defn parse-input [in]
  (->> (str/split-lines in)
       (map #(str/split % #"-"))
       (mapcat (fn [[a b]]
                 [{a [b]}
                  {b [a]}]))
       (apply merge-with concat)
       (map-vals (comp vec #(remove #{"start"} %)))))

(defn big-cave? [cave]
  (= cave (str/upper-case cave)))

(defn compute-paths [legal-move? grouped-paths]
  (loop [cursor "start"
         trail []
         history #{}]
    (let [trail (conj trail cursor)]
     (if (= "end" cursor)
       (recur "start" [] (conj history trail))
       (let [moves (->> (get grouped-paths cursor)
                        ;; remove illegal moves
                        (filter (partial legal-move? trail))
                        ;; remove moves we already tried
                        (remove (fn [to]
                                  (contains? history (conj trail to)))))]
         (cond
           (and (= 1 (count trail))
                (empty? moves))
           ;; return recorded paths that ended
           (filter (comp #{"end"} last) history)

           (empty? moves)
           (recur "start" [] (conj history trail))

           :else
           (let [[to & other-moves] moves] ;; take first legal move
             (recur to trail history))))))))

(defn legal-move-1? [trail to]
  (cond
    (big-cave? to) true
    :else (not-any? #{to} trail)))

(defn solve-1 [input]
  (->> (parse-input input)
       (compute-paths legal-move-1?)
       (count)))

(defn legal-move-2? [trail to]
  (cond
    (big-cave? to)
    true

    (#{"start" "end"} to)
    (not-any? #{to} trail)

    :else (or (not-any? #{to} trail)
              (->> (remove big-cave? trail)
                   (frequencies)
                   (vals)
                   (every? #{1})))))

(defn solve-2 [input]
  (->> (parse-input input)
       (compute-paths legal-move-2?)
       (count)))

(comment
 ; {:end #{}, :start #{:A :b}, :A #{:c :b :end}, :b #{:d :end}}
 (parse-input example-input)

 ; 5157
 (time (solve-1 puzzle-input))
 ; 144309
 (time (solve-2 puzzle-input))

 ; 36
 (solve-2 example-input))

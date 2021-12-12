(ns day6
  (:require [clojure.set :as set]))

(def example-input [3,4,3,1,2])
(def puzzle-input  [1,1,1,1,1,5,1,1,1,5,1,1,3,1,5,1,4,1,5,1,2,5,1,1,1,1,3,1,4,5,1,1,2,1,1,1,2,4,3,2,1,1,2,1,5,4,4,1,4,1,1,1,4,1,3,1,1,1,2,1,1,1,1,1,1,1,5,4,4,2,4,5,2,1,5,3,1,3,3,1,1,5,4,1,1,3,5,1,1,1,4,4,2,4,1,1,4,1,1,2,1,1,1,2,1,5,2,5,1,1,1,4,1,2,1,1,1,2,2,1,3,1,4,4,1,1,3,1,4,1,1,1,2,5,5,1,4,1,4,4,1,4,1,2,4,1,1,4,1,3,4,4,1,1,5,3,1,1,5,1,3,4,2,1,3,1,3,1,1,1,1,1,1,1,1,1,4,5,1,1,1,1,3,1,1,5,1,1,4,1,1,3,1,1,5,2,1,4,4,1,4,1,2,1,1,1,1,2,1,4,1,1,2,5,1,4,4,1,1,1,4,1,1,1,5,3,1,4,1,4,1,1,3,5,3,5,5,5,1,5,1,1,1,1,1,1,1,1,2,3,3,3,3,4,2,1,1,4,5,3,1,1,5,5,1,1,2,1,4,1,3,5,1,1,1,5,2,2,1,4,2,1,1,4,1,3,1,1,1,3,1,5,1,5,1,1,4,1,2,1])

;; example 18 days: 26
;; example 80 days: 5934
;; part1: 374994
(time
 (loop [round  0
        fishes example-input]
   (if (= round 18)
     (count fishes)
     (recur (inc round)
            (reduce (fn [fishes' ^long fish]
                      (cond
                        (= 0 fish)
                        (conj fishes' 6 8)

                        :else
                        (conj fishes' (dec fish))))
                    []
                    fishes)))))

(time
 (loop [round  0
        fishes (frequencies puzzle-input)]
   (if (= round 256)
     (apply + (vals fishes))
     (recur (inc round)
            (-> (dissoc fishes 0)
                (set/rename-keys {8 7
                                  7 6
                                  6 5
                                  5 4
                                  4 3
                                  3 2
                                  2 1
                                  1 0})
                (update 6 (fnil + 0) (get fishes 0 0))
                (update 8 (fnil + 0) (get fishes 0 0)))))))

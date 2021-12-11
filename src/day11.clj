(ns day11
  (:require [clojure.string :as str]))

(def example-input "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526\n")
(def example-input2 "11111\n19991\n19191\n19991\n11111")
(def puzzle-input "4721224663\n6875415276\n2742448428\n4878231556\n5684643743\n3553681866\n4788183625\n4255856532\n1415818775\n2326886125")

(defn parse-input [in]
  (->> (str/split-lines in)
       (map #(str/split % #""))
       (reduce (fn [m row]
                 (conj m (mapv #(Long/parseLong %) row)))
               [])))

;; borrowed some day 9 functions
(defn bounds [m]
  {:minX 0
   :minY 0
   :maxX (dec (count (first m)))
   :maxY (dec (count m))})

(defn in-bounds? [{:keys [minX maxX minY maxY]} {:keys [x y]}]
  (and
   (>= maxY y)
   (>= maxX x)
   (<= minY y)
   (<= minX x)))

(defn coordinates [{:keys [minX maxX minY maxY]}]
  (for [y (range minY (inc maxY))
        x (range minX (inc maxX))]
    {:x x :y y}))

;; There's probably a proper method to get all neighbours of a coord, but this is pretty effective
(defn neighbours [{:keys [x y]} bounds]
  (->> [{:x (dec x) :y y}
        {:x (inc x) :y y}
        {:x x :y (dec y)}
        {:x x :y (inc y)}
        {:x (inc x) :y (inc y)}
        {:x (inc x) :y (dec y)}
        {:x (dec x) :y (inc y)}
        {:x (dec x) :y (dec y)}]
       (filter (partial in-bounds? bounds))))

(defn inc-things
  ([m flashed coords]
   (reduce (fn [{:keys [m flashed] :as memo} {:keys [x y] :as coord}]
             (cond
               (and (= 9 (get-in m [y x]))
                   (not (flashed coord)))
               (inc-things (assoc-in m [y x] 0)
                           (conj flashed coord)
                           (neighbours coord (bounds m)))

               (not (flashed coord))
               (update-in memo [:m y x] inc)

               :else memo))
           {:flashed flashed
            :m       m}
           coords)))

(defn solve-1 [in]
  (let [m (parse-input in)
        bounds (bounds m)
        coords (coordinates bounds)]
    (loop [round 0
           m m
           flashes 0]
      (if (= 100 round)
        flashes
        (let [x (inc-things m #{} coords)]
         (recur (inc round)
                (:m x)
                (+ flashes (count (:flashed x)))))))))

(defn solve-2 [in]
  (let [m (parse-input in)
        bounds (bounds m)
        coords (coordinates bounds)]
    (loop [round 0
           m m
           flashes 0]
      (if (= #{0} (set (flatten m)))
        round
        (let [x (inc-things m #{} coords)]
         (recur (inc round)
                (:m x)
                (+ flashes (count (:flashed x)))))))))

(comment
 (parse-input example-input)

 (def m (parse-input example-input))
 ((neighbours {:x 1, :y 1} (bounds m)))

 ;; 1675
 (solve-1 puzzle-input)

 ;; 515
 (solve-2 puzzle-input))

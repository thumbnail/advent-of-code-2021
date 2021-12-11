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

(defn coordinates [matrix]
  (let [{:keys [minX maxX minY maxY]} (bounds matrix)]
   (for [y (range minY (inc maxY))
         x (range minX (inc maxX))]
     {:x x :y y})))

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

(defn step
  ([matrix coords]
   (step matrix coords #{}))
  ([matrix coords flashed]
   (reduce (fn [{:keys [matrix flashed] :as memo} {:keys [x y] :as coord}]
             (cond
               (and (= 9 (get-in matrix [y x]))
                   (not (flashed coord)))
               (step (assoc-in matrix [y x] 0)
                     (neighbours coord (bounds matrix))
                     (conj flashed coord))

               (not (flashed coord))
               (update-in memo [:matrix y x] inc)

               :else memo))
           {:flashed flashed
            :matrix  matrix}
           coords)))

(defn perform-steps [matrix stop-pred]
  (let [coords (coordinates matrix)]
    (loop [round 0
           matrix matrix
           flashes 0]
      (if (stop-pred {:round round :matrix matrix})
        {:flashes flashes
         :round round
         :matrix matrix}
        (let [{:keys [matrix flashed]} (step matrix coords)]
          (recur (inc round) matrix (+ flashes (count flashed))))))))

(defn solve-1 [in]
  (-> (parse-input in)
      (perform-steps (comp #{100} :round))
      (:flashes)))

(defn solve-2 [in]
  (-> (parse-input in)
      (perform-steps (comp #{#{0}} set flatten :matrix))
      (:round)))

(comment
 (parse-input example-input)

 (def m (parse-input example-input))
 (neighbours {:x 1, :y 1} (bounds m))

 ;; 1675
 (solve-1 puzzle-input)

 ;; 515
 (solve-2 puzzle-input))

(ns day17)

(def example-input "target area: x=20..30, y=-10..-5")
(def puzzle-input "target area: x=195..238, y=-93..-67")

(defn parse [in]
  (->> (re-matches #"target area: x=(-?[0-9]+)..(-?[0-9]+), y=(-?[0-9]+)..(-?[0-9]+)" in)
       (rest)
       (map #(Long/parseLong %))
       (zipmap [:minX :maxX :minY :maxY])))

(defn hit? [{:keys [x y]} {:keys [minX minY maxX maxY]}]
  (and (<= minX x maxX)
       (<= minY y maxY)))

(defn velocity-step [x]
  (if (zero? x)
    0
    (if (< x 0)
      (inc x)
      (dec x))))

(defn fire [{:keys [x y] :as coord} {x' :x y' :y :as velocity}]
  (lazy-seq
   (cons coord
         (fire (-> coord
                   (update :x + x')
                   (update :y + y'))
               (-> velocity
                   (update :y dec)
                   (update :x velocity-step))))))

(defn passed? [{:keys [x y]} {:keys [minX minY maxX maxY]}]
  (or (< maxX x)
      (> minY y)))

(defn options [{:keys [maxX minY] :as target}]
  (for [x (range 0 (inc maxX))
        y (range 100 (dec minY) -1) ;; guestimate
        :let [trajectory (take-while #(not (passed? % target))
                                     (fire {:x 0, :y 0} {:x x :y y}))]
        :when (hit? (last trajectory) target)]
    trajectory))

(defn solve-1 [in]
  (->> (options (parse in))
       (mapcat #(map :y %))
       (apply max)))

(defn solve-2 [in]
  (count (options (parse in))))

(comment
 (def target (parse example-input))

 (options target)

 (hit? (last (take-while (fn [x]
                           (println x)
                           (or (not (passed? x target))
                               (hit? x target)))
                         (fire {:x 0, :y 0} {:x 17, :y -4})))
       target)

 (hit? {:x 25 :y -5} target)
 (passed? {:x 31 :y -7})

 ;; 45
 (solve-1 example-input)

 ;; 4278
 (solve-1 puzzle-input)

 ;; 112
 (solve-2 example-input)

 ;; 1994
 (solve-2 puzzle-input))

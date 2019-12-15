(require '[clojure.math.combinatorics :as combo])

(defn replace-at [seq index replacement]
  (concat (take index seq) (list replacement) (drop (+ index 1) seq)))

(defn new-position [x y z] {:x x :y y :z z})
(defn new-velocity
  ([] {:x 0 :y 0 :z 0})
  ([x y z] {:x x :y y :z z}))
(defn new-moon
  ([position] {:position position :velocity (new-velocity)})
  ([position velocity] {:position position :velocity velocity}))
(defn potential-energy [moon]
  (+ 
    (Math/abs (:x (:position moon)))
    (Math/abs (:y (:position moon)))
    (Math/abs (:z (:position moon)))))
(defn kinetic-energy [moon]
  (+ 
    (Math/abs (:x (:velocity moon)))
    (Math/abs (:y (:velocity moon)))
    (Math/abs (:z (:velocity moon)))))
(defn total-energy [moon] (* (potential-energy moon) (kinetic-energy moon)))
(defn total-energy-in-system [moons]
  (reduce + (map total-energy moons)))

(defn apply-gravity-between [m1 m2]
  (let [
    m1x (:x (:position m1)) m2x (:x (:position m2))
    m1y (:y (:position m1)) m2y (:y (:position m2))
    m1z (:z (:position m1)) m2z (:z (:position m2))
    v1x (:x (:velocity m1)) v2x (:x (:velocity m2))
    v1y (:y (:velocity m1)) v2y (:y (:velocity m2))
    v1z (:z (:velocity m1)) v2z (:z (:velocity m2))
    ]
    [(new-moon
       (:position m1)
       (new-velocity
         (cond
           (< m1x m2x) (+ v1x 1)
           (= m1x m2x) v1x
           :else (- v1x 1))
         (cond
           (< m1y m2y) (+ v1y 1)
           (= m1y m2y) v1y
           :else (- v1y 1))
         (cond
           (< m1z m2z) (+ v1z 1)
           (= m1z m2z) v1z
           :else (- v1z 1))))
     (new-moon
       (:position m2)
       (new-velocity
         (cond
           (< m1x m2x) (- v2x 1)
           (= m1x m2x) v2x
           :else (+ v2x 1))
         (cond
           (< m1y m2y) (- v2y 1)
           (= m1y m2y) v2y
           :else (+ v2y 1))
         (cond
           (< m1z m2z) (- v2z 1)
           (= m1z m2z) v2z
           :else (+ v2z 1))))
     ]))
  
(defn apply-gravity
  ([moons] (apply-gravity moons (combo/combinations (range (count moons)) 2)))
  ([moons pair-indices]
    (if (empty? pair-indices)
      moons
      (let [
        [i1 i2] (first pair-indices)
        [m1 m2] (apply-gravity-between (nth moons i1) (nth moons i2))
        ]
        (apply-gravity
          (replace-at (replace-at moons i1 m1) i2 m2)
          (rest pair-indices))))))

(defn apply-velocity-single [moon]
  (new-moon
    (new-position
      (+ (:x (:position moon)) (:x (:velocity moon)))
      (+ (:y (:position moon)) (:y (:velocity moon)))
      (+ (:z (:position moon)) (:z (:velocity moon))))
    (:velocity moon)))
(defn apply-velocity [moons]
  (map apply-velocity-single moons))

(defn simulate-motion [moons steps]
  (if (<= steps 0)
    moons
    (simulate-motion
      (apply-velocity (apply-gravity moons))
      (- steps 1))))

(defn read-position [line]
  (let [
    [_ x y z]
    (re-matches
      #"<x=(-?\d+), y=(-?\d+), z=(-?\d+)>"
      line)
    ]
    (if x 
      (new-position  
        (Integer/parseInt x) 
        (Integer/parseInt y)
        (Integer/parseInt z))
      (throw
        (IllegalArgumentException.
          (format "Ill-formatted position: %s" line))))))

(defn read-positions [raw-position-lines]
  (map read-position raw-position-lines))

(defn day12-part1 [file-name]
  (with-open
    [positions-in (clojure.java.io/reader file-name)]
    (let [
      lines (doall (line-seq positions-in))
      moons (map (comp new-moon read-position) lines)
      ]
      (total-energy-in-system (simulate-motion moons 1000)))))


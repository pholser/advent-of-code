(require '[clojure.string :as str])

(defn x [coord] (first coord))
(defn y [coord] (second coord))
(defn r [coord] (first coord))
(defn theta [coord] (second coord))

(defn translate-coord [[origin-x origin-y] [x y]]
  [(- x origin-x) (- y origin-y)])
(defn square [x] (* x x))
(defn distance [cart]
  (Math/sqrt (+ (square (x cart)) (square (y cart)))))
(defn polar-angle [cart]
  (let [
    angle (Math/atan2 (x cart) (- (y cart)))
    ]
    (if (< angle 0) (+ angle (* 2 Math/PI)) angle)))
(defn cartesian->polar [cart]
  (let [r (distance cart) theta (polar-angle cart)] [r theta]))

(defn ys-with-xs [rows]
  (map-indexed vector (map #(map-indexed vector %) rows)))

; ([0 \.] [1 \#] [2 \.] [3 \.] [4 \#])
(defn asteroid-xs [xs]
  (map first (filter #(= (second %) \#) xs)))

; [0 ([0 \.] [1 \#] [2 \.] [3 \.] [4 \#])]
(defn asteroid-coordinates-in-row [[y xs]]
  (map (fn [x] [x y]) (asteroid-xs xs)))

(defn asteroid-coordinates [rows]
  (apply concat (map asteroid-coordinates-in-row (ys-with-xs rows))))

; For each asteroid a:
;   Check each other asteroid b to see if, on the line between a and b,
;     any other asteroid intervenes. If not, a has clear line-of-sight to b.

(defn collinear? [a b c]
  (=
    (* (- (x b) (x a)) (- (y c) (y a)))
    (* (- (x c) (x a)) (- (y b) (y a)))))
(defn within? [p q r]
   (and
     (or (<= p q r) (<= r q p))
     (or (not= p r) (= p q r))))
(defn between? [a c b]
  (and
    (collinear? a b c)
    (if (= (x a) (x b))
      (within? (y a) (y c) (y b))
      (within? (x a) (x c) (x b)))))
(defn clear-line-of-sight? [a b asteroids]
  (=
    0
    (count
      (filter
        (fn [c] (and (not= a c) (not= b c) (between? a c b)))
        asteroids))))
(defn visible-from [a asteroids]
  (filter #(clear-line-of-sight? a % asteroids) asteroids))
(defn location-for-monitoring-station [asteroids]
  (apply max-key (comp count #(visible-from % asteroids)) asteroids))

(defn asteroids-by-polar-angle [paired-coords]
  (group-by (fn [item] (theta (:polar item))) paired-coords))
(defn asteroids-by-polar-angle-and-distance [paired-coords]
  (let [
    groups (asteroids-by-polar-angle paired-coords)
    dist (fn [a1 a2] (compare (r (:polar a1)) (r (:polar a2))))]
    (into (sorted-map)
      (for [[angle asteroids] groups]
        [angle (sort dist asteroids)]))))
(defn destruction-order [asteroid-groups]
  (if
    (empty? asteroid-groups)
    '()
    (concat
      (map first (vals asteroid-groups))
      (foo
        (into
          (sorted-map)
          (filter
            (fn [[dist asteroids]] ((complement empty?) asteroids))
            (map
              (fn [[k vs]] [k (rest vs)])
              asteroid-groups)))))))
 
(defn part1-answer [asteroids]
  (apply max
    (map (comp count #(visible-from % asteroids)) asteroids)))

(defn day10-part1 [file-name]
  (with-open
    [rows-in (clojure.java.io/reader file-name)]
    (let [rows (doall (line-seq rows-in))]
      (part1-answer (asteroid-coordinates rows)))))

(defn day10-part2 [file-name]
  (with-open
    [rows-in (clojure.java.io/reader file-name)]
    (let [
      rows (doall (line-seq rows-in))
      cart-asteroids-all (asteroid-coordinates rows)
      monitoring-station (location-for-monitoring-station cart-asteroids-all)
      cart-asteroids-no-monitor
        (remove (partial = monitoring-station) cart-asteroids-all)
      polar-asteroids
        (map
          (comp cartesian->polar (partial translate-coord monitoring-station))
          cart-asteroids-no-monitor)
      paired-coords
        (map
          (fn [[c p]] {:cart c :polar p})
          (partition 2 (interleave cart-asteroids-no-monitor polar-asteroids)))
      asteroid-groups (asteroids-by-polar-angle-and-distance paired-coords)
      order (destruction-order asteroid-groups)
      two-hundredth (:cart (nth order 199 "no such asteroid"))]
      (+ (* 100 (x two-hundredth)) (y two-hundredth)))))


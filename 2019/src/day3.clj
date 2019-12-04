(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combos])

(defn point [x y] {:x x :y y})
(defn manhattan-distance [p1 p2]
  (+ (Math/abs (- (:x p1) (:x p2))) (Math/abs (- (:y p1) (:y p2)))))
(defn segment [p1 p2] {:p1 p1 :p2 p2})
(defn segment-horizontal? [s]
  (= (:y (:p1 s)) (:y (:p2 s))))
(defn segment-vertical? [s]
  (= (:x (:p1 s)) (:x (:p2 s))))

(defn segments-parallel? [s1 s2]
  (or
    (and (segment-horizontal? s1) (segment-horizontal? s2))
    (and (segment-vertical? s1) (segment-vertical? s2))))

(defn segment-intersection [[s1 s2]]
  (cond
    (segments-parallel? s1 s2) []
    (segment-horizontal? s1)
      (if
        (and 
          (or (<= (:y (:p1 s2)) (:y (:p1 s1)) (:y (:p2 s2)))
              (<= (:y (:p2 s2)) (:y (:p1 s1)) (:y (:p1 s2))))
          (or (<= (:x (:p1 s1)) (:x (:p1 s2)) (:x (:p2 s1)))
              (<= (:x (:p2 s1)) (:x (:p1 s2)) (:x (:p1 s1)))))
        (point (:x (:p1 s2)) (:y (:p1 s1)))
        [])
    :else
      (if
        (and
          (or (<= (:x (:p1 s2)) (:x (:p1 s1)) (:x (:p2 s2)))
              (<= (:x (:p2 s2)) (:x (:p1 s1)) (:x (:p1 s2))))
          (or (<= (:y (:p1 s1)) (:y (:p1 s2)) (:y (:p2 s1)))
              (<= (:y (:p2 s1)) (:y (:p1 s2)) (:y (:p1 s1)))))
        (point (:x (:p1 s1)) (:y (:p1 s2)))
        [])))

(defn move [direction steps] {:direction direction :steps steps})
(def origin (point 0 0))

(defn read-move [raw-move]
  (move (symbol (subs raw-move 0 1)) (Integer/parseInt (subs raw-move 1))))

(defn read-moves [raw-moves]
  (map read-move (str/split raw-moves #",")))

(defn move->segment [init-pos move]
  (let
    [dir (:direction move)
      steps (:steps move)]
    (cond
      (= dir 'R)
        (segment init-pos (point (+ (:x init-pos) steps) (:y init-pos))) 
      (= dir 'L)
        (segment init-pos (point (- (:x init-pos) steps) (:y init-pos))) 
      (= dir 'U)
        (segment init-pos (point (:x init-pos) (+ (:y init-pos) steps)))
      (= dir 'D)
        (segment init-pos (point (:x init-pos) (- (:y init-pos) steps))))))

(defn moves->wirepos [init-pos moves]
  (if (empty? moves)
    '()
    (let [first-segment (move->segment init-pos (first moves))]
      (cons
        first-segment
        (moves->wirepos (:p2 first-segment) (rest moves))))))

(defn moves->wire [moves]
  (moves->wirepos origin moves))

(defn intersections [[wire1 wire2]]
  (filter
    (fn [point] (and ((complement empty?) point) ((complement =) point origin)))
    (map segment-intersection (combos/cartesian-product wire1 wire2))))

(defn day3-part1 []
  (with-open [wires-in (clojure.java.io/reader "./src/wires.txt")]
    (apply min
      (map
        (partial manhattan-distance origin)
        (intersections
          (map (comp moves->wire read-moves) (line-seq wires-in)))))))


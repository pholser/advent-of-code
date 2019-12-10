(require '[clojure.string :as str])

(defn x [coord] (first coord))
(defn y [coord] (second coord))

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
(defn part1-answer [asteroids]
  (apply max
    (map (comp count #(visible-from % asteroids)) asteroids)))

(defn day10-part1 [file-name]
  (with-open
    [rows-in (clojure.java.io/reader file-name)]
    (let [rows (doall (line-seq rows-in))]
      (part1-answer (asteroid-coordinates rows)))))


(require '[clojure.string :as str])

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

(defn day10-part1 [file-name]
  (with-open
    [rows-in (clojure.java.io/reader file-name)]
    (let [
      rows (doall (line-seq rows-in))
      ]
      (asteroid-coordinates rows))))


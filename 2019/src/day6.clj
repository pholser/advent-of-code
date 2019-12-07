(require '[clojure.string :as str])

(defn edge [from to] {:from from :to to})
(defn new-directed-graph [] {})
(defn new-graph [] {})
(defn graph-has-node? [g n]
  ((complement nil?) (get g n)))
(defn add-directed-edge [g e]
  (if (graph-has-node? g (:from e))
    (throw
      (IllegalArgumentException.
        (format
          "Already have direct orbit from %s: %s. Trying to add %s"
          (:from e)
          (get g (:from e))
          (:to e))))
    (assoc g (:from e) (:to e))))
(defn add-edge [g e]
  (if (graph-has-node? g (:from e))
    (assoc g (:from e) (conj (get g (:from e)) (:to e)))
    (assoc g (:from e) [(:to e)])))

(defn distance-to-root-from [g n]
  (if
    ((complement graph-has-node?) g n)
    0
    (+ 1 (distance-to-root-from g (get g n)))))

(defn total-direct-and-indirect-orbits [g]
  (reduce + (map (partial distance-to-root-from g) (keys g))))

(defn dijkstra
  ([g s]
    (dijkstra
      g
      s
      (merge (zipmap (keys g) (repeat 99999999)) {s 0})
      (keys g)))
  ([g s dist not-visited]
    (if (empty? not-visited)
      dist
      (let [u (first (apply min-key second (select-keys dist not-visited)))
             neighbors (get g u)]
        (dijkstra
          g
          s
          (merge dist
            (apply assoc {}
              (interleave
                neighbors
                (map
                  (fn [v] (min (get dist v) (+ 1 (get dist u))))
                  neighbors))))
          (remove #{u} not-visited))))))

(defn orbits->directed-graph [orbits g]
  (if (empty? orbits) g
    (let [[orbitee orbiter] (first orbits)]
      (orbits->directed-graph
        (rest orbits)
        (add-directed-edge g (edge orbiter orbitee)))))) 

(defn orbits->graph [orbits g]
  (if (empty? orbits) g
    (let [[orbitee orbiter] (first orbits)]
      (orbits->graph
        (rest orbits)
        (add-edge
          (add-edge g (edge orbiter orbitee))
          (edge orbitee orbiter))))))

(defn read-orbits [seq]
  (map (fn [line] (str/split line #"[)]")) seq))

(defn day6-part1 []
  (with-open
    [orbits-in (clojure.java.io/reader "src/day6-input.txt")]
    (total-direct-and-indirect-orbits
      (orbits->directed-graph 
        (read-orbits (doall (line-seq orbits-in)))
        (new-directed-graph)))))

(defn day6-part2 [file-name]
  (with-open
    [orbits-in (clojure.java.io/reader file-name)]
    (let [orbits (doall (line-seq orbits-in))
           digraph
             (orbits->directed-graph (read-orbits orbits) (new-graph))
           graph
             (orbits->graph (read-orbits orbits) (new-graph))]
      (get (dijkstra graph (get digraph "YOU")) (get digraph "SAN")))))


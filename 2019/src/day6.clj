(require '[clojure.string :as str])

(defn edge [from to] {:from from :to to})
(defn new-graph [] {})
(defn graph-has-node? [g n]
  ((complement nil?) (get g n)))
(defn add-edge [g e]
  (if (graph-has-node? g (:from e))
    (throw
      (IllegalArgumentException.
        (format
          "Already have direct orbit from %s: %s. Trying to add %s"
          (:from e)
          (get g (:from e))
          (:to e))))
    (assoc g (:from e) (:to e))))

(defn distance-to-root-from [g n]
  (if
    ((complement graph-has-node?) g n)
    0
    (+ 1 (distance-to-root-from g (get g n)))))

(defn total-direct-and-indirect-orbits [g]
  (reduce + (map (partial distance-to-root-from g) (keys g))))

(defn orbits->graph [orbits g]
  (if (empty? orbits) g
    (let [[orbitee orbiter] (first orbits)]
      (orbits->graph (rest orbits) (add-edge g (edge orbiter orbitee)))))) 

(defn read-orbits [seq]
  (map (fn [line] (str/split line #"[)]")) seq))

(defn day6-part1 []
  (with-open
    [orbits-in (clojure.java.io/reader "src/day6-input.txt")]
    (total-direct-and-indirect-orbits
      (orbits->graph 
        (read-orbits (doall (line-seq orbits-in)))
        (new-graph)))))


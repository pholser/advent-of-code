(require '[clojure.string :as str])

(defn digit-count [digit layer]
  (count (filter (partial = digit) (:pixels layer))))

(defn layer-with-fewest-zeros-among [layers]
  (apply min-key (partial digit-count 0) layers))

(defn layer [width height pixels]
  {:pixels pixels :width width :height height})

(defn picture [width height pixels]
  {:pixels pixels :width width :height height})

(defn render-pixel [pixel]
  (if (= pixel 0) " " "*"))

(defn render-row [pixels]
  (str/join (map render-pixel pixels)))

(defn render-picture [pic]
  (run! println (map render-row (partition (:width pic) (:pixels pic)))))

(defn merge-pixels [p1 p2]
  (if (< p1 2) p1 p2))

(defn merge-picture [layers]
  (if
    (empty? layers)
    (picture 0 0 '())
    (let [
      fst (first layers)
      width (:width fst)
      height (:height fst)]
      (picture
        width
        height
        (map
          (partial reduce merge-pixels)
          (partition
            (count layers)
            (apply interleave (map (fn [l] (:pixels l)) layers))))))))

(defn digits->layers [digits width height]
  (map
    (partial layer width height)
    (partition
      (* width height)
      (map
        #(Integer/parseInt %)
        (mapcat (fn [s] (str/split s #"")) digits)))))

(defn day8-part1 [file-name width height]
  (with-open
    [digits-in (clojure.java.io/reader file-name)]
    (let [
      digits (doall (line-seq digits-in))
      layers (digits->layers digits width height)
      fewest-zero-layer (layer-with-fewest-zeros-among layers)]
      (*
        (digit-count 1 fewest-zero-layer)
        (digit-count 2 fewest-zero-layer)))))

(defn day8-part2 [file-name width height]
  (with-open
    [digits-in (clojure.java.io/reader file-name)]
    (let [
      digits (doall (line-seq digits-in))
      layers (digits->layers digits width height)
      pic (merge-picture layers)]
      (render-picture pic))))


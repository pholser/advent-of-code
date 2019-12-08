(require '[clojure.string :as str])

(defn digit-count-row [digit row]
  (count (filter (partial = digit) row))) 
  
(defn digit-count [digit layer]
  (reduce + (map (partial digit-count-row digit) layer)))

(defn layer-with-fewest-zeros-among [layers]
  (apply min-key (partial digit-count 0) layers))

(defn digits->layers [digits width height]
  (partition height
    (partition width
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


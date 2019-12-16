(require '[clojure.java.io :as io])
(require '[clojure.string :as str])

(defn new-reaction [ingredients product]
  {:ingredients ingredients :product product})
(defn parse-ingredient [raw-ingredient]
  (let [[raw-quantity raw-chemical] (str/split raw-ingredient #" " 2)]
    {:quantity (Integer/parseInt raw-quantity)
     :chemical raw-chemical}))
(defn parse-ingredients [raw-ingredients]
  (map parse-ingredient (str/split raw-ingredients #", ")))
(defn read-reaction [line]
  (let [
    [raw-ingredients raw-product] (str/split line #" => ")
    ingredients (parse-ingredients raw-ingredients)
    product (parse-ingredient raw-product)
    ]
    (new-reaction ingredients product)))

(defn day14-part1 [file-name]
  (with-open
    [reactions-in (clojure.java.io/reader file-name)]
    (let [
      lines (doall (line-seq reactions-in)) 
      reactions (map read-reaction lines)
      ]
      reactions)))

(defn six-digits? [n] (<= 100000 n 999999))
(defn has-adjacent-same-digits? [n]
  (some (fn [[d1 d2]] (= d1 d2)) (partition 2 1 (str n))))
(defn increasing-digits? [n]
  (every?
    (fn [[d1 d2]] (<= (compare d1 d2) 0))
    (partition 2 1 (str n))))

(defn inclusive-range [start end] (range start (inc end)))

(defn day4-part1 []
  (count
    (filter
      (every-pred six-digits? has-adjacent-same-digits? increasing-digits?)
      (inclusive-range 138307 654504))))

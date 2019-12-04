(defn six-digits? [n] (<= 100000 n 999999))
(defn has-adjacent-same-digits? [n]
  (some (fn [[d1 d2]] (= d1 d2)) (partition 2 1 (str n))))
(defn increasing-digits? [n]
  (every?
    (fn [[d1 d2]] (<= (compare d1 d2) 0))
    (partition 2 1 (str n))))
(defn part2-stipulation? [n]
  (let [adjacents (map first (re-seq #"(.)\1*" (str n)))]
    (> (count (filter (fn [s] (= 2 (count s))) adjacents)) 0)))

(defn inclusive-range [start end] (range start (inc end)))

(defn day4-part1 []
  (count
    (filter
      (every-pred six-digits? has-adjacent-same-digits? increasing-digits?)
      (inclusive-range 138307 654504))))

(defn day4-part2 []
  (count
    (filter
      (every-pred
        six-digits?
        has-adjacent-same-digits?
        increasing-digits?
        part2-stipulation?)
      (inclusive-range 138307 654504))))


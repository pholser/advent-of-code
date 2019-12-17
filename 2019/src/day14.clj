(require '[clojure.java.io :as io])
(require '[clojure.string :as str])

(defn new-reaction [ingredients product]
  {:ingredients ingredients :product product})

(defn parse-ingredient [raw-ingredient]
  (let [[raw-quantity raw-chemical] (str/split raw-ingredient #" " 2)]
    [raw-chemical (Integer/parseInt raw-quantity)]))
(defn parse-product [raw-product]
  (let [[raw-quantity raw-chemical] (str/split raw-product #" " 2)]
    {:chemical raw-chemical :output-qty (Integer/parseInt raw-quantity)}))
(defn parse-ingredients [raw-ingredients]
  (into (hash-map)
    (map parse-ingredient (str/split raw-ingredients #", "))))

(defn read-reaction [line]
  (let [
    [raw-ingredients raw-product] (str/split line #" => ")
    ingredients (parse-ingredients raw-ingredients)
    product (parse-product raw-product)
    ]
    (new-reaction ingredients product)))

(defn reactions-by-product [reactions]
  (into (hash-map)
    (map
      (fn [r]
        [
          (:chemical (:product r))
          {:output-qty (:output-qty (:product r))
           :ingredients (:ingredients r)}
        ]
      )
      reactions)))

(defn run-reaction-in-reverse [reactions chem qty]
  (let [
    r (get reactions chem)
    o (:output-qty r)
    ]
    (into (hash-map)
      (for [[c q] (:ingredients r)]
        [c (* q (int (Math/ceil (/ qty o))))]))))
(defn directly-reducible-to-ore? [chem reactions]
  (let [r (get reactions chem)]
    (= ["ORE"] (keys (:ingredients r)))))

; (amt / (get c (:ingredients r)), rounded up, * output-qty
(defn reduce-to-ore [chem-qty reactions]
  (loop [cq chem-qty rs reactions]
    (cond
      (every? (fn [c] (directly-reducible-to-ore? c rs)) (keys cq))
      (reduce + 
        (map
          (fn [c] (get (run-reaction-in-reverse rs c (get cq c)) "ORE"))
          (keys cq)))

    :else
      (recur
        (let [
          cs
          (group-by (fn [c] (directly-reducible-to-ore? c rs)) (keys cq))
          ]
          (apply
            (partial merge-with +)
            (flatten [
              (select-keys cq (get cs true))
              (map (fn [c] (run-reaction-in-reverse rs c (get cq c)))
                (get cs false))
            ])))
        rs))))

(defn day14-part1 [file-name]
  (with-open
    [reactions-in (clojure.java.io/reader file-name)]
    (let [
      lines (doall (line-seq reactions-in)) 
      reactions (map read-reaction lines)
      ]
      (reactions-by-product reactions))))


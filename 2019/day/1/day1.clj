(defn fuel-for-module-launch [mass]
  (- (quot mass 3) 2))

(defn corrected-fuel-for-module-launch [mass]
  (let [base-fuel (fuel-for-module-launch mass)]
    (if (neg? base-fuel)
      0
      (+ base-fuel (corrected-fuel-for-module-launch base-fuel)))))

(defn total-fuel-requirements [masses]
  (reduce + masses))

(defn day1 [fuel-calc]
  (with-open [masses-in (clojure.java.io/reader "./masses.txt")]
    (total-fuel-requirements
      (map
        (comp fuel-calc #(Integer/parseInt %))
        (line-seq masses-in)))))

(defn day1-part1 [] (day1 fuel-for-module-launch))
(defn day1-part2 [] (day1 corrected-fuel-for-module-launch))


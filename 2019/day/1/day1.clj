(defn fuel-for-module-launch [mass]
  (- (quot mass 3) 2))

(defn total-fuel-requirements [masses]
  (reduce + masses))

(defn day1 []
  (with-open [masses-in (clojure.java.io/reader "./masses.txt")]
    (total-fuel-requirements
      (map
        (comp fuel-for-module-launch #(Integer/parseInt %))
        (line-seq masses-in))))) 

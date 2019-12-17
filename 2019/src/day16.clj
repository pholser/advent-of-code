(require '[clojure.java.io :as io])
(require '[clojure.string :as str])

(defn read-fft-digits [line]
  (map #(Integer/parseInt %) (str/split line #"")))

(defn calculate-fft-pattern [index length]
  (let [reps (+ index 1)]
    (rest
      (take
        (+ 1 length)
        (flatten
          (repeat [
            (repeat reps 0)
            (repeat reps 1)
            (repeat reps 0)
            (repeat reps -1)]))))))

(defn execute-fft-phase [digits]
  (map
    (fn [[index digit]]
      (let [
        pattern (calculate-fft-pattern index (count digits))
        ]
        (Math/abs (rem (reduce + (map * digits pattern)) 10))))
    (map-indexed vector digits)))

(defn execute-fft [digits number-of-phases]
  (loop [ds digits np number-of-phases]
    (if (= 0 np)
      (take 8 ds) 
      (recur (execute-fft-phase ds) (- np 1)))))

(defn day16-part1 [file-name num-phases]
  (with-open
    [digits-in (clojure.java.io/reader file-name)]
    (let [
      line (first (doall (line-seq digits-in)))
      digits (read-fft-digits line)
      ]
      (doall (execute-fft digits num-phases)))))


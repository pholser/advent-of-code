(require '[clojure.string :as str])

(defn replace-at [seq index replacement]
  (concat (take index seq) (list replacement) (drop (+ index 1) seq)))

(defn value-of-memory-cell [program pos] (nth program pos))
(defn value [program arg] (Integer/parseInt arg))
(defn value-at [program arg]
  (value program (value-of-memory-cell program (Integer/parseInt arg))))
(defn parameter-mode [indicator]
  (case indicator
    0 value-at
    1 value
    :else
      (throw
        (IllegalArgumentException.
          (format "Unrecognized parameter mode: %d" indicator)))))
(defn mode-at [modes pos] (nth modes pos value-at))
(defn arg-at [program start modes index]
  ((mode-at modes index) program (nth program (+ start (+ index 1)))))
(defn store-at [program pos val] (replace-at program pos (str val)))

(defn execute-arith-op [op program start modes]
  (let [inval1 (arg-at program start modes 0)
         inval2 (arg-at program start modes 1)
         outpos (Integer/parseInt (value-of-memory-cell program (+ start 3)))
         result (op inval1 inval2)]
    (store-at program outpos result)))

(defn execute-store-op [program start modes]
  (let [val (Integer/parseInt (read-line))
         outpos (Integer/parseInt (value-of-memory-cell program (+ start 1)))]
    (store-at program outpos val)))

(defn execute-print-op [program start modes]
  (let [val (arg-at program start modes 0)]
    (do
      (println val)
      program)))

(defn parse-instruction [instruction]
  (case instruction
    "1" {:opcode "01" :modes [value-at value-at]}
    "2" {:opcode "02" :modes [value-at value-at]}
    "3" {:opcode "03" :modes []}
    "4" {:opcode "04" :modes [value-at]}
    "99" {:opcode "99" :modes []}
    (let [divider (- (count instruction) 2)]
      {:opcode (subs instruction divider)
        :modes
          (map
            (comp parameter-mode #(Integer/parseInt %))
            (reverse (map str (subs instruction 0 divider))))
      })))

(defn run-at [program start]
  (let [instruction (parse-instruction (nth program start))]
    (case (:opcode instruction)
      "01"
        (run-at
          (execute-arith-op + program start (:modes instruction)) (+ start 4))
      "02"
        (run-at
          (execute-arith-op * program start (:modes instruction)) (+ start 4))
      "03"
        (run-at
          (execute-store-op program start (:modes instruction)) (+ start 2))
      "04"
        (run-at
          (execute-print-op program start (:modes instruction)) (+ start 2))
      "99" program
      (throw
        (IllegalArgumentException.
          (format "Weird opcode at %d: %s" start (:opcode instruction))))))) 

(defn run [program]
  (first (run-at program 0)))

(defn read-program [seq]
  (mapcat (fn [line] (str/split line #",")) seq))

(defn day5-part1 []
  (with-open
    [program-in (clojure.java.io/reader "src/day5-part1-program.txt")]
    (run (read-program (doall (line-seq program-in))))))


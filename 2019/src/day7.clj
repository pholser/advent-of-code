(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combos])
(require '[clojure.core.async :as a])

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

(defn execute-store-op [program input start modes]
  (let [val (Integer/parseInt (a/<!! input))
         outpos (Integer/parseInt (value-of-memory-cell program (+ start 1)))]
    (store-at program outpos val)))

(defn execute-print-op [program output start modes]
  (let [val (arg-at program start modes 0)]
    (do
      (a/>!! output (str val))
      program)))

(defn jump-dest [op program start modes]
  (let [val (arg-at program start modes 0)
         newptr (arg-at program start modes 1)]
    (if
      (op 0 val)
      newptr
      (+ start 3))))

(defn execute-compare-op [op program start modes]
  (let [inval1 (arg-at program start modes 0)
         inval2 (arg-at program start modes 1)
         outpos (Integer/parseInt (value-of-memory-cell program (+ start 3)))]
    (store-at program outpos (if (op inval1 inval2) "1" "0"))))

(defn parse-instruction [instruction]
  (case instruction
    "1" {:opcode "01" :modes [value-at value-at]}
    "2" {:opcode "02" :modes [value-at value-at]}
    "3" {:opcode "03" :modes []}
    "4" {:opcode "04" :modes [value-at]}
    "5" {:opcode "05" :modes [value-at value-at]}
    "6" {:opcode "06" :modes [value-at value-at]}
    "7" {:opcode "07" :modes [value-at value-at]}
    "8" {:opcode "08" :modes [value-at value-at]}
    "99" {:opcode "99" :modes []}
    (let [divider (- (count instruction) 2)]
      {:opcode (subs instruction divider)
        :modes
          (map
            (comp parameter-mode #(Integer/parseInt %))
            (reverse (map str (subs instruction 0 divider))))
      })))

(defn run-at [program input output start]
  (let [instruction (parse-instruction (nth program start))]
    (case (:opcode instruction)
      "01"
        (run-at
          (execute-arith-op + program start (:modes instruction))
          input
          output
          (+ start 4))
      "02"
        (run-at
          (execute-arith-op * program start (:modes instruction))
          input
          output
          (+ start 4))
      "03"
        (run-at
          (execute-store-op program input start (:modes instruction))
          input
          output
          (+ start 2))
      "04"
        (run-at
          (execute-print-op program output start (:modes instruction))
          input
          output
          (+ start 2))
      "05"
        (run-at
          program
          input
          output
          (jump-dest not= program start (:modes instruction)))
      "06"
        (run-at
           program
           input
           output
           (jump-dest = program start (:modes instruction)))
      "07"
        (run-at
          (execute-compare-op < program start (:modes instruction))
          input
          output
          (+ start 4))
      "08"
        (run-at
          (execute-compare-op = program start (:modes instruction))
          input
          output
          (+ start 4))
      "99" program
      (throw
        (IllegalArgumentException.
          (format "Weird opcode at %d: %s" start (:opcode instruction)))))))

(defn run [program input output]
  (first (run-at program input output 0)))

(defn read-program [seq]
  (mapcat (fn [line] (str/split line #",")) seq))

(defn amplifier [program input output]
  {:program program :input input :output output})

(defn amp-input [amp value]
  (a/>!! (:input amp) (str value)))

(defn amp-run [amp]
  (run (:program amp) (:input amp) (:output amp)))
  
(defn day7 [file-name phase-min phase-max]
  (with-open
    [program-in (clojure.java.io/reader file-name)]
    (let [instructions (doall (line-seq program-in))]
      (apply max
        (map
          (fn [[ph1 ph2 ph3 ph4 ph5]]
            (let [
              in (a/chan 10)
              chan12 (a/chan 10)
              chan23 (a/chan 10)
              chan34 (a/chan 10)
              chan45 (a/chan 10)
              amp1 (amplifier (read-program instructions) in chan12)
              amp2 (amplifier (read-program instructions) chan12 chan23)
              amp3 (amplifier (read-program instructions) chan23 chan34)
              amp4 (amplifier (read-program instructions) chan34 chan45)
              amp5 (amplifier (read-program instructions) chan45 in)]
              (do
                (amp-input amp1 ph1)
                (amp-input amp1 0)
                (amp-input amp2 ph2)
                (amp-input amp3 ph3)
                (amp-input amp4 ph4)
                (amp-input amp5 ph5)
                (a/thread (amp-run amp1))
                (a/thread (amp-run amp2))
                (a/thread (amp-run amp3))
                (a/thread (amp-run amp4))
                (amp-run amp5)
                (Integer/parseInt (a/<!! (:output amp5))))))
          (combos/permutations (range phase-min (+ 1 phase-max))))))))


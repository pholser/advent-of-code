(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combos])

(defn replace-at [seq index replacement]
  (concat (take index seq) (list replacement) (drop (+ index 1) seq)))

(defn execute-op [op program start]
  (let [inpos1 (nth program (+ start 1))
         inpos2 (nth program (+ start 2))
         outpos (nth program (+ start 3))
         result (op (nth program inpos1) (nth program inpos2))]
    (replace-at program outpos result)))

(defn run-at [program start]
  (let [opcode (nth program start)]
    (cond
      (= opcode 1) (run-at (execute-op + program start) (+ start 4))
      (= opcode 2) (run-at (execute-op * program start) (+ start 4))
      (= opcode 99) program
      :else
        (throw
          (IllegalArgumentException.
            (format "Weird opcode at %d: %d" start opcode))))))

(defn run [program noun verb]
  (first (run-at (replace-at (replace-at program 2 verb) 1 noun) 0)))

(defn read-program [seq]
  (map
    #(Integer/parseInt %)
    (mapcat (fn [line] (str/split line #",")) seq)))

(defn day2-part1 []
  (with-open [program-in (clojure.java.io/reader "src/1202-program-alarm.txt")]
    (run (read-program (line-seq program-in)) 12 2)))

(defn day2-part2 []
  (with-open [program-in (clojure.java.io/reader "src/1202-program-alarm.txt")]
    (let [program (read-program (line-seq program-in))
         [result-noun result-verb]
           (first
             (filter
               (fn [pair]
                 (let [[noun verb] pair]
                   (= 19690720 (run program noun verb))))
               (combos/cartesian-product (range 100) (range 100))))]
         (+ (* 100 result-noun) result-verb))))


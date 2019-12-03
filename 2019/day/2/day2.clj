(require '[clojure.string :as str])

(defn execute-op [op program start]
  (let [inpos1 (nth program (+ start 1))
         inpos2 (nth program (+ start 2))
         outpos (nth program (+ start 3))
         result (op (nth program inpos1) (nth program inpos2))]
    (concat
      (take outpos program)
      [result]
      (drop (+ outpos 1) program))))

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

(defn run [program]
  (run-at program 0))

(defn read-program [seq]
  (map
    #(Integer/parseInt %)
    (mapcat (fn [line] (str/split line #",")) seq)))

(defn day2-part1 []
  (with-open [program-in (clojure.java.io/reader "./1202-program-alarm.txt")]
    (run (read-program (line-seq program-in)))))


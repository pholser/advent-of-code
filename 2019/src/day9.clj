(require '[clojure.string :as str])
(require '[clojure.core.async :as a])

(defn replace-at [seq index replacement]
  (concat (take index seq) (list replacement) (drop (+ index 1) seq)))

(defn value-of-memory-cell [program pos] (nth program pos))

; interpreted mode 1, literal mode 0, literal mode 1
(defn value [program arg relative-base] (biginteger arg))

; interpreted mode 0
(defn value-at [program arg relative-base]
  (value
    program
    (value-of-memory-cell program (biginteger arg))
    relative-base))

; literal mode 2
(defn value-relative [program arg relative-base]
  (+ (biginteger arg) (biginteger relative-base)))

; interpreted mode 2
(defn value-at-relative [program arg relative-base]
  (value-at
    program
    (value-relative program arg relative-base)
    relative-base)) 

(defn interpreted-parameter-mode [indicator default-mode]
  (case (str indicator)
    "0" value-at
    "1" value
    "2" value-at-relative
    "" default-mode
    :else
      (throw
        (IllegalArgumentException.
          (format "Unrecognized parameter mode: %d" indicator)))))

(defn literal-parameter-mode [indicator default-mode]
  (case (str indicator)
    "0" value
    "1" value
    "2" value-relative
    "" default-mode
    :else
      (throw
        (IllegalArgumentException.
          (format "Unrecognized parameter mode: %d" indicator)))))
(defn mode-at [instruction pos] (nth (:modes instruction) pos))
(defn arg-at [program pc relative-base instruction index]
  ((mode-at instruction index)
    program
    (nth program (+ pc (+ index 1)))
    relative-base))
(defn store-at [program pos val] (replace-at program pos (str val)))

(defn execute-arith-op [op program pc relative-base instruction]
  (let [
    inval1 (arg-at program pc relative-base instruction 0)
    inval2 (arg-at program pc relative-base instruction 1)
    outpos (arg-at program pc relative-base instruction 2)
    result (op inval1 inval2)]
    (do
      (println "storing " inval1 op inval2 " = " result " at addr " outpos) 
      (store-at program outpos result))))

(defn execute-store-op [program input pc relative-base instruction]
  (let [
    val (a/<!! input)
    outpos (arg-at program pc relative-base instruction 0)]
    (do
      (println "storing " val " at addr " outpos)
      (store-at program outpos val))))

(defn execute-print-op [program output pc relative-base instruction]
  (let [val (arg-at program pc relative-base instruction 0)]
    (do
      (println "print modes" (:modes instruction))
      (println "printing " val)
      (a/>!! output (str val))
      program)))

(defn jump-dest [op program pc relative-base instruction]
  (let [
    val (arg-at program pc relative-base instruction 0)
    newptr (arg-at program pc relative-base instruction 1)
    cmp (op 0 val)
    dest (if cmp newptr (+ pc 3))]
    (do
      (println "since " val op " 0 = " cmp ", jumping to addr " dest)
      dest)))

(defn execute-compare-op [op program pc relative-base instruction]
  (let [
    inval1 (arg-at program pc relative-base instruction 0)
    inval2 (arg-at program pc relative-base instruction 1)
    outpos (arg-at program pc relative-base instruction 2)
    cmp (op inval1 inval2)
    result (if cmp "1" "0")]
    (do
      (println "since " inval1 op inval2 " = " cmp
        ", storing " result " at addr " outpos)
      (store-at program outpos result))))

(defn adjust-relative-base [program pc relative-base instruction]
  (do (println "adjust-relative-base, rel=" relative-base
         ", " (:modes instruction)) 
  (let [
    delta (arg-at program pc relative-base instruction 0)
    newbase (+ relative-base delta)]
    (do
      (println "adjusting relative base to " relative-base " + " delta " = "
        newbase)
      newbase)))   )

(defn parse-instruction [instruction]
  (let [
    int-instruction (Integer/parseInt instruction)
    int-opcode (mod int-instruction 100)
    int-modes (quot int-instruction 100)
    str-modes (str/reverse (format "%d" int-modes))
    ]
    (case int-opcode
      1 {:opcode "01"
         :modes [
           (interpreted-parameter-mode (nth str-modes 0 "") value-at)
           (interpreted-parameter-mode (nth str-modes 1 "") value-at)
           (literal-parameter-mode (nth str-modes 2 "") value)
         ]
        }
      2 {:opcode "02"
         :modes [
           (interpreted-parameter-mode (nth str-modes 0 "") value-at)
           (interpreted-parameter-mode (nth str-modes 1 "") value-at)
           (literal-parameter-mode (nth str-modes 2 "") value)
         ]
        }
      3 {:opcode "03"
         :modes [
           (literal-parameter-mode (nth str-modes 0 "") value)
         ]
        }
      4 {:opcode "04"
         :modes [
           (interpreted-parameter-mode (nth str-modes 0 "") value-at)
         ]
        }
      5 {:opcode "05"
         :modes [
           (interpreted-parameter-mode (nth str-modes 0 "") value-at)
           (literal-parameter-mode (nth str-modes 1 "") value)
         ]
        }
      6 {:opcode "06"
         :modes [
           (interpreted-parameter-mode (nth str-modes 0 "") value-at)
           (literal-parameter-mode (nth str-modes 1 "") value)
         ]
        }
      7 {:opcode "07"
         :modes [
           (interpreted-parameter-mode (nth str-modes 0 "") value-at)
           (interpreted-parameter-mode (nth str-modes 1 "") value-at)
           (literal-parameter-mode (nth str-modes 2 "") value)
         ]
        }
      8 {:opcode "08"
         :modes [
           (interpreted-parameter-mode (nth str-modes 0 "") value-at)
           (interpreted-parameter-mode (nth str-modes 1 "") value-at)
           (literal-parameter-mode (nth str-modes 2 "") value)
         ]
        }
      9 {:opcode "09"
         :modes [
           (interpreted-parameter-mode (nth str-modes 0 "") value)
         ]
        }
      99 {:opcode "99" :modes []}
      :else {:opcode instruction :modes []}
    )))

(defn run-at [program input output pc relative-base]
  (do (println "pc = " pc ", relative base = " relative-base)
  (let [instruction (parse-instruction (nth program pc))]
    (case (:opcode instruction)
      "01"
        (run-at
          (execute-arith-op + program pc relative-base instruction)
          input
          output
          (+ pc 4)
          relative-base)
      "02"
        (run-at
          (execute-arith-op * program pc relative-base instruction)
          input
          output
          (+ pc 4)
          relative-base)
      "03"
        (run-at
          (execute-store-op program input pc relative-base instruction)
          input
          output
          (+ pc 2)
          relative-base)
      "04"
        (run-at
          (execute-print-op program output pc relative-base instruction)
          input
          output
          (+ pc 2)
          relative-base)
      "05"
        (run-at
          program
          input
          output
          (jump-dest not= program pc relative-base instruction)
          relative-base)
      "06"
        (run-at
           program
           input
           output
           (jump-dest = program pc relative-base instruction)
           relative-base)
      "07"
        (run-at
          (execute-compare-op < program pc relative-base instruction)
          input
          output
          (+ pc 4)
          relative-base)
      "08"
        (run-at
          (execute-compare-op = program pc relative-base instruction)
          input
          output
          (+ pc 4)
          relative-base)
      "09"
        (run-at
          program
          input
          output
          (+ pc 2)
          (adjust-relative-base program pc relative-base instruction))
      "99" program
      (throw
        (IllegalArgumentException.
          (format "Weird opcode at %d: %s" pc (:opcode instruction)))))))

)

(defn run [program input output]
  (first (run-at program input output 0 0)))

(defn read-program [seq]
  (concat
    (mapcat (fn [line] (str/split line #",")) seq)
    (take 100000 (repeat "0"))))

(defn day9 [file-name]
  (with-open
    [program-in (clojure.java.io/reader file-name)]
    (let [
      instructions (doall (line-seq program-in))
      in (a/chan 100)
      out (a/chan 100) 
      program (read-program instructions)]
      (do
        (a/>!! in "1")
        (run program in out)
        (loop [line (a/poll! out)]
          (if line
            (do
              (println line)
              (recur (a/poll! out)))
            '()))))))


(require '[clojure.java.io :as io])
(require '[clojure.string :as str])

(defn next-output [computer]
  (.readOutput computer))
(defn has-more-output? [computer]
  (.peek (.outBuffer computer)))

(defn tile-content-for-id [id]
  (case id
    "0" :empty
    "1" :wall
    "2" :block
    "3" :paddle
    "4" :ball))

(defn add-tile-to-board [board coord tile]
  (assoc board coord tile))
(defn board-block-count [board]
  (count (filter #(= :block %) (vals board))))
(defn new-game-board
  ([computer] (new-game-board computer (sorted-map)))
  ([computer board]
    (loop [c computer b board]
      (if (not (has-more-output? c))
        b
        (let [
          x (Integer/parseInt (next-output c))
          y (Integer/parseInt (next-output c))
          tile-contents (tile-content-for-id (next-output c))
          ]
          (recur
            c
            (add-tile-to-board b [x y] tile-contents)))))))
 
(defn day13-part1 []
  (let [
    inbuf (new java.util.concurrent.LinkedBlockingQueue)
    outbuf (new java.util.concurrent.LinkedBlockingQueue)
    computer (new com.pholser.intcode.IntcodeComputer 20000 inbuf outbuf)
    program-in (io/input-stream "src/day13-input.txt")
    ]
    (do
      (.loadProgram computer program-in) 
      (.run computer)
      (board-block-count (new-game-board computer)))))

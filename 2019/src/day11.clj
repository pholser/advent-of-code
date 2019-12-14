(require '[clojure.core.async :as a])
(require '[clojure.java.io :as io])
(require '[clojure.math.combinatorics :as combo])
(require '[clojure.string :as str])

(defn new-panel [] (sorted-map))

(defn new-robot [computer panel position orientation]
  {:computer computer :panel panel :position position :orientation orientation})

(defn position [x y] (vector x y))
(defn x [pos] (first pos))
(defn y [pos] (second pos))
(defn code-for-color [color]
  (case color
    :black "0"
    :white "1"))
(defn color-for-code [code]
  (case code
    "0" :black
    "1" :white))
(defn turn-for-code [code]
  (case code
    "0" :left
    "1" :right))

(defn color-at-position [panel pos]
  (get panel pos :black))

(defn paint-panel [panel pos color]
  (do (println "painting" pos "the color" color)
  (assoc panel pos color))
)

(defn calc-orientation [old-orientation turn]
  (case turn
    :left
    (case old-orientation
      :up :left
      :left :down
      :down :right
      :right :up)
    :right
    (case old-orientation
      :up :right
      :right :down
      :down :left
      :left :up)))

(defn calc-position [old-position orientation]
  (case orientation
    :up [(x old-position) (+ 1 (y old-position))]
    :right [(+ 1 (x old-position)) (y old-position)]
    :down [(x old-position) (- (y old-position) 1)]
    :left [(- (x old-position) 1) (y old-position)]))

(defn robot-running? [robot]
  (.runningProgram (:computer robot)))
(defn robot-send-input [robot value]
  (.feedInput (:computer robot) value))
(defn robot-read-output [robot]
  (.readOutput (:computer robot)))
  
(defn robot-send-paint-color [robot]
  (robot-send-input
    robot 
    (code-for-color (color-at-position (:panel robot) (:position robot)))))

(defn robot-read-next-paint-color-and-turn [robot]
  (do
    (let [
      new-color (color-for-code (robot-read-output robot))
      new-turn (turn-for-code (robot-read-output robot))]
      [new-color new-turn])))

(defn paint-ship [robot]
  (loop [r robot]
    (if (not (robot-running? r))
      r
      (do
        (robot-send-paint-color r)
        (let [
          [new-color new-turn] (robot-read-next-paint-color-and-turn r)
          new-orientation (calc-orientation (:orientation r) new-turn)
          new-position (calc-position (:position r) new-orientation)]
          (recur
            (new-robot
              (:computer r)
              (paint-panel (:panel r) (:position r) new-color)
              new-position
              new-orientation)))))))

(defn day11-part1 []
  (let [
    inbuf (new java.util.concurrent.LinkedBlockingQueue)
    outbuf (new java.util.concurrent.LinkedBlockingQueue)
    computer (new com.pholser.intcode.IntcodeComputer 20000 inbuf outbuf)
    program-in (io/input-stream "src/day11-input.txt")
    panel (new-panel)
    robot (new-robot computer panel (position 0 0) :up) 
    ]
    (do
      (.loadProgram computer program-in) 
      (a/thread (.run computer))
      (count (:panel (paint-ship robot))))))

(defn normalize-panel-coords [panel]
  (let [
    min-x (apply min (map (fn [[x y]] x) (keys panel)))
    min-y (apply min (map (fn [[x y]] y) (keys panel)))
    ]
    (into
      (new-panel)
      (for [[coord color] panel]
        [ (position
            (+ (x coord) (max 0 (- min-x)))
            (+ (y coord) (max 0 (- min-y))))
          color]))))

(defn render-panel [panel]
  (let [
    max-x (apply max (map (fn [[x y]] x) (keys panel)))
    max-y (apply max (map (fn [[x y]] y) (keys panel)))
    ]
    (map
      (partial str/join "")
      (partition
        (+ 1 max-y)
        (map
          (fn [coord]
            (case (get panel coord :black)
              :black " "
              :white "*"))
          (map
            #(apply vector %)
            (combo/cartesian-product (range (+ 1 max-x)) (range (+ 1 max-y)))))))))

(defn day11-part2 []
  (let [
    inbuf (new java.util.concurrent.LinkedBlockingQueue)
    outbuf (new java.util.concurrent.LinkedBlockingQueue)
    computer (new com.pholser.intcode.IntcodeComputer 20000 inbuf outbuf)
    program-in (io/input-stream "src/day11-input.txt")
    panel (paint-panel (new-panel) (position 0 0) :white)
    robot (new-robot computer panel (position 0 0) :up) 
    ]
    (do
      (.loadProgram computer program-in) 
      (a/thread (.run computer))
      (render-panel (normalize-panel-coords (:panel (paint-ship robot)))))))

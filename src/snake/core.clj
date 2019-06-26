(ns snake.core
  (:require [clojure.string :as s]
            [quil.core :as q])
  (:import java.awt.event.KeyEvent)
  (:gen-class))

;; -------------------------------------------------------------------------------------------
;; ----------------------------------------- GLOBALS -----------------------------------------
;; -------------------------------------------------------------------------------------------

;; Is the game active?
(def GAME-RUNNING? (atom false))

;; Has the player lost the game?
(def GAME-OVER? (atom false))

;; Can be one of :left, :right, :up, :down.
(def MOVE-DIRECTION (atom :left))

;; The number of apples the player has eaten so far.
(def NUM-APPLES (atom 0))

(def LAST-MOVE-TIME (atom (System/currentTimeMillis))) ;; The exact time of when the snake last moved.

;; This file will persist the player's high score.
(def HIGH-SCORE-FILE "./score.dat")

;; GUI Options.
(def WINDOW-WIDTH 500)
(def WINDOW-HEIGHT 500)
(def SQUARE-WIDTH 20)
(def SQUARE-HEIGHT 20)
(def NUM-ROWS (/ WINDOW-WIDTH SQUARE-WIDTH))
(def NUM-COLS (/ WINDOW-HEIGHT SQUARE-HEIGHT))
(def FPS 60)

(def EMPTY-LINE
  (into
    (vector)
    (take NUM-COLS (repeat "."))))

(defn get-empty-playfield
  "Contract: nil -> vector<vector>"
  []
  (into [] (take NUM-ROWS (repeat EMPTY-LINE))))

(def PLAY-FIELD (atom (get-empty-playfield)))

;; The snake is represented by a seq of maps (which contain x,y coordinates).
;; The first element of the seq is the head of the snake and the rest are the body.
;; The snake is initially 2 squares long, starting at position x=15,y=15
(def SNAKE
  (atom '({:row 15 :col 15} {:row 15 :col 16})))

;; -------------------------------------------------------------------------------------------
;; ---------------------------------------- COLLISION ----------------------------------------
;; -------------------------------------------------------------------------------------------

(defn is-in-bounds?
  "Returns true if the given x,y coordinates (ints) are within the playfield, false otherwise.
  The playfield is a matrix (vector of vectors)."
  [row col play-field]
  (let [max-row (count play-field)
        max-col (count (first play-field))
        min-row 0
        min-col 0]
    (cond
      (< row min-row) false
      (< col min-col) false
      (>= row max-row) false
      (>= col max-col) false
      :else true)))

(defn is-in-snake?
  "Returns true if the given x,y coordinates (ints) are inside the snake, false otherwise.
  The snake is a seq like this: '({:row 100 :col 100} {:row 100 :col 101} ...)"
  [row col snake]
  (> (count
       (filter #(and (= row (:row %)) (= col (:col %))) snake))
     0))

(defn has-apple?
  "Returns true if the playfield currently contains an apple.
  The playfield is a matrix (vector of vectors)."
  [play-field]
  (> (count (filter #(.contains % "a") play-field)) 0))

(defn check-collision
  "This function checks if the snake in the game has hit anything while moving.
  The snake is a seq like this: '({:row 100 :col 100} {:row 100 :col 101} ...)
  The playfield is a matrix (vector of vectors).
  Returns a keyword which represents what we hit, possible values:
  :no-collision, :side-collision, :snake-collision, :apple-collision."
  [snake play-field]
  (let [snake-head (first snake)
        snake-body (rest snake)
        snake-row (:row snake-head)
        snake-col (:col snake-head)]
    (cond
      (= false (is-in-bounds? snake-row snake-col play-field)) :side-collision
      (= true (is-in-snake? snake-row snake-col snake-body)) :snake-collision
      (= "a" (get-in play-field [snake-row snake-col])) :apple-collision
      :else :no-collision)))

;; -------------------------------------------------------------------------------------------
;; ----------------------------------------- MOVEMENT ----------------------------------------
;; -------------------------------------------------------------------------------------------

(defn move-head
  "Moves the head of the snake in the specified direction.
  The head is a map of x,y coordinates, that looks like this: {:row 100 :col 100}.
  The direction is one of these keywords: :left :right :up :down.
  The function returns a new map with the :row, :col updated."
  [head direction]
  (case direction
    :left  (update-in head [:col] dec)
    :right (update-in head [:col] inc)
    :up    (update-in head [:row] dec)
    :down  (update-in head [:row] inc)))

(defn move-snake
  "Moves the snake by one square in the specified direction.
  The snake is a seq like this: '({:row 100 :col 100} {:row 100 :col 101} ...)
  The direction is one of these keywords: :left :right :up :down.
  The function returns a new seq with all rows and cols moved."
  [snake direction]
  (let [head (first snake)
        new-head (move-head head direction)
        snake-with-tail-chopped (butlast snake)]
    (when (not= new-head (second snake)) ;; The snake is not allowed to backtrack.
      (cons new-head snake-with-tail-chopped))))

(defn expand-snake
  "Grows the snake by one square in the specified direction.
  The snake is a seq like this: '({:row 100 :col 100} {:row 100 :col 101} ...)
  The direction is one of these keywords: :left :right :up :down.
  The function returns a new seq with one new element added."
  [snake direction]
  (let [head (first snake)
        new-head (move-head head direction)]
    (cons new-head snake)))

(defn insert-snake
  "Adds a snake to the playfield (the snake is represented by the string 's').
  The playfield is a matrix (vector of vectors).
  The snake is a seq like this: '({:row 100 :col 100} {:row 100 :col 101} ...)"
  [snake play-field]
  ;; A mutable approach is best here.
  (let [temp-field (atom play-field)]
    (doseq [part snake]
      (swap! temp-field
             (fn [x]
               (assoc-in x [(:row part) (:col part)] "s"))))
    @temp-field))

(defn insert-apple
  "Adds a apple to the playfield (the apple is represented by the string 'a').
  The playfield is a matrix (vector of vectors).
  The row and col are ints."
  [play-field row col]
  (assoc-in play-field [row col] "a"))

;; -------------------------------------------------------------------------------------------
;; ------------------------------------------ SCORE ------------------------------------------
;; -------------------------------------------------------------------------------------------

(defn read-high-score
  "string -> int
  Reads the players best score from a file"
  [fname]
  (clojure.edn/read-string (slurp fname)))

(defn save-high-score
  "string int -> nil
  Saves the players score to a file."
  [fname score]
  (spit fname (with-out-str (pr score))))

(defn overwrite-high-score!
  "string int -> nil
  Overwrites the players high score if the current score exceeds the highscore."
  [fname score]
  (let [high-score (read-high-score fname)]
    (when (> score high-score)
      (save-high-score fname score))))

;; -------------------------------------------------------------------------------------------
;; ------------------------------------------- GUI -------------------------------------------
;; -------------------------------------------------------------------------------------------

(defn setup []
  (q/frame-rate FPS)
  (q/stroke 0)
  (q/stroke-weight 0)
  (q/background 255 255 255))

(defn get-color
  [ch]
  (cond
    (= \s ch) (q/fill 120 120 120)
    (= \a ch) (q/fill 255 17 0)
    :else (q/fill 255 255 255)))

(defn print-line!
  [text lnum use-color]
  (doall
    (map-indexed
      (fn [idx ch]
        (get-color ch)
        (q/rect (* idx SQUARE-WIDTH) (* lnum SQUARE-HEIGHT) SQUARE-WIDTH SQUARE-HEIGHT))
      text)))

(defn print-matrix!
  [matrix offset]
  (if (empty? matrix) (recur (get-empty-playfield) offset)
    (let [lines (map #(s/join "" %) matrix)]
      (doseq [[line i] (map list lines (range (count lines)))]
        (print-line! line (+ i offset) true)))))

(defn get-key []
  (let [raw-key (q/raw-key)
        the-key-code (q/key-code)
        the-key-pressed (if (= processing.core.PConstants/CODED (int raw-key)) the-key-code raw-key)]
    the-key-pressed))

(defn update-direction [direction]
  (when (move-snake @SNAKE direction) ;; Try moving to the given direction and see if it returns nil.
    (swap! MOVE-DIRECTION (fn [x] direction))))

(defn read-input []
  (let [user-input (get-key)]
    (cond
      (= KeyEvent/VK_LEFT user-input)  (update-direction :left)
      (= KeyEvent/VK_RIGHT user-input) (update-direction :right)
      (= KeyEvent/VK_DOWN user-input)  (update-direction :down)
      (= KeyEvent/VK_UP user-input)    (update-direction :up)
      (= \newline user-input)          (swap! GAME-RUNNING? not))))

(defn show-pause-menu! []
  (q/background 173 216 230)
  (q/stroke 0 0 0)
  (q/fill 0 0 0)
  (q/text-size 20)
  (q/text-align :center)
  (q/text "SNAKE" (/ WINDOW-WIDTH 2) 50)
  (q/text "MOUSE CLICK OR ENTER: PLAY/PAUSE" (/ WINDOW-WIDTH 2) 90)
  (q/text (str "APPLES: " @NUM-APPLES) (/ WINDOW-WIDTH 2) 120)
  (q/text (str "HIGH SCORE: " (read-high-score HIGH-SCORE-FILE)) (/ WINDOW-WIDTH 2) 150))

(defn show-game-over-screen! []
  (q/background 0 0 0)
  (q/stroke 255 255 255)
  (q/fill 255 255 255)
  (q/text-size 20)
  (q/text-align :center)
  (q/text "GAME OVER" (/ WINDOW-WIDTH 2) 50)
  (q/text (str "APPLES: " @NUM-APPLES) (/ WINDOW-WIDTH 2) 90)
  (q/text (str "HIGH SCORE: " (read-high-score HIGH-SCORE-FILE)) (/ WINDOW-WIDTH 2) 150))

;; -------------------------------------------------------------------------------------------
;; ---------------------------------------- GAME LOOP ----------------------------------------
;; -------------------------------------------------------------------------------------------

(defn get-all-positions
  "Returns all coordinates on the playfield as a set.
  The set contains vectors of two integers that represent the coordinates.
  Example result: #{[0 0] [0 1] [0 2] ...}"
  []
  (into
    #{}
    (for [x (range NUM-ROWS) y (range NUM-COLS)]
      [x y])))

(defn get-corners
  "Returns a seq with the 4 corners of the playfield.
  The corners are represented as vectors of two ints (x y positions)."
  []
  (let [first-row 0
        first-col 0
        last-row (dec NUM-ROWS)
        last-col (dec NUM-COLS)]
    (conj [] [first-row first-col] [last-row last-col] [first-row last-col] [last-row first-col])))

(defn get-valid-coordinates
  "Returns a seq with all positions on the playfield that don't contain the snake.
  Also, the corners of the playfield are excluded as they are very hard to get by."
  []
  (let [all-positions (get-all-positions)
        snake-positions (map #(vector (:row %) (:col %)) @SNAKE)
        corners (get-corners)
        corner1 (nth corners 0)
        corner2 (nth corners 1)
        corner3 (nth corners 2)
        corner4 (nth corners 3)]
    (clojure.set/difference
      all-positions
      (into #{} (conj snake-positions corner1 corner2 corner3 corner4)))))

(defn remove-apple [play-field]
  (let [remove-apple-row (fn [x] (into [] (map #(if (= "a" %) "." %) x)))]
    (into [] (map remove-apple-row play-field))))

(defn place-apple!
  "Places an apple on the playfield if there isn't one already.
  The playfield should be a mutable matrix (atom), so that the apple is persistent."
  [play-field-atom]
  (when (not (has-apple? @play-field-atom))
    (let [chosen-position (first (shuffle (get-valid-coordinates)))
          row (first chosen-position)
          col (second chosen-position)]
      (swap! play-field-atom #(insert-apple % row col)))))

(defn eat-apple!
  "When the snake eats an apple 3 things happen:
  1. The apple is erased from the playfield.
  2. The snake expands by one square.
  3. The NUM-APPLES variable is increased by one."
  [snake-atom play-field-atom]
  (swap! play-field-atom #(remove-apple %))
  (swap! snake-atom #(expand-snake % @MOVE-DIRECTION))
  (swap! NUM-APPLES inc))

(defn game-loop! []
  ;; The snake moves one square in the current direction.
  (swap! SNAKE #(move-snake % @MOVE-DIRECTION))
  ;; Place an apple on the playfield if there isn't one already.
  (place-apple! PLAY-FIELD)
  ;; Handle any collisions.
  (let [collision (check-collision @SNAKE @PLAY-FIELD)]
    (cond
      (= :apple-collision collision) (eat-apple! SNAKE PLAY-FIELD)
      (= :snake-collision collision) (swap! GAME-OVER? (fn [x] true))
      (= :side-collision collision)  (swap! GAME-OVER? (fn [x] true)))))

(defn draw-playfield!
  "Basically calls the game loop and draws the snake and playfield every 120 milliseconds."
  []
  (when (>
         (- (System/currentTimeMillis) @LAST-MOVE-TIME)
         120)
    (reset! LAST-MOVE-TIME (System/currentTimeMillis))
    (game-loop!)
    (print-matrix! (insert-snake @SNAKE @PLAY-FIELD) 0)))

(defn show-game! []
  (cond
    (= true @GAME-OVER?) (do (show-game-over-screen!) (overwrite-high-score! HIGH-SCORE-FILE @NUM-APPLES))
    (= false @GAME-RUNNING?) (show-pause-menu!)
    :else (draw-playfield!)))

;; -------------------------------------------------------------------------------------------
;; ------------------------------------------ MAIN -------------------------------------------
;; -------------------------------------------------------------------------------------------

(defn -main []
  (q/defsketch snake
    :title "Snake"
    :settings #(q/smooth 2)
    :setup setup
    :renderer :opengl
    :key-pressed read-input
    :mouse-clicked #(swap! GAME-RUNNING? not)
    :draw show-game!
    :features [:exit-on-close]
    :size [WINDOW-WIDTH WINDOW-HEIGHT]))

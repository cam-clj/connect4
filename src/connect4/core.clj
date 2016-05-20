(ns connect4.core
  (:require [clojure.string :as str]))

(def n-cols 7)
(def n-rows 6)

(def init-state {:board   (vec (repeat n-cols []))
                 :to-move :red
                 :winner  nil})

(defn counter
  [x]
  (case x
    :red "x"
    :yellow "o"
    "."))

(defn transpose [x] (apply mapv vector x))

(defn show-state
  [{:keys [board to-move] :as state}]
  (doseq [r (range (dec n-rows) -1 -1)]
    (println
     (str/join " "
               (map (fn [c] (counter (get-in board [c r])))
                    (range n-cols)))))
  (println (str/join " " (range n-cols))))

(defn winning-move-for?
  [player]
  (fn [part] (every? #{player} part)))

(let [rows (for [r (range n-rows)
                 c (range (- n-cols 3))]
             (map (fn [c] [c r]) (take 4 (iterate inc c))))
      cols (for [c (range n-cols)
                 r (range (- n-rows 3))]
             (map (fn [r] [c r]) (take 4 (iterate inc r))))
      lr-diags (for [c (range (- n-cols 3))
                     r (range (- n-rows 3))]
                 (map vector
                      (take 4 (iterate inc c))
                      (iterate inc r)))
      rl-diags (for [c (range (dec n-cols) 2 -1)
                     r (range (dec n-rows) 2 -1)]
                 (map vector
                      (take 4 (iterate dec c))
                      (iterate dec r)))]
  (def winning-positions
    (concat rows cols rl-diags lr-diags)))

(defn expand-winning-positions
  [board]
  (map (fn [coords] (map (partial get-in board) coords))
       winning-positions))

(defn check-winner
  [{:keys [board] :as state} candidate]
  (if (some (winning-move-for? candidate)
            (expand-winning-positions board))
    (assoc state :winner candidate)
    state))

(defn toggle-player
  [u]
  (case u :red :yellow :yellow :red))

(defn valid-move?
  [board c]
  (and c (< (count (board c)) n-rows)))

(defn make-move
  [{:keys [board to-move] :as state} c]
  {:pre [(valid-move? board c)]}
  (-> state
      (update-in [:board c] conj to-move)
      (update :to-move toggle-player)
      (check-winner to-move)))

(let [moves (into {} (map (fn [x] [(str x) x]) (range n-cols)))]
  (defn read-move
    []
    (let [x (read-line)]
      (when-let [move (moves x)]
        move))))

(defn solicit-move
  [state]
  (println (str (counter (:to-move state)) " to move: "))
  (let [move (read-move)]
    (if (valid-move? (:board state) move)
      move
      (do
        (println "That is not a valid move!")
        (recur state)))))

(defn game-loop
  [state]
  (show-state state)
  (if-let [winner (:winner state)]
    (println "Game over! " (counter winner) " has won.")
    (let [move (solicit-move state)]
      (recur (make-move state move)))))

(defn new-game
  []
  (game-loop init-state))

(defn parse-board
  [s]
  (transpose
   (reverse
    (partition n-cols
               (map #(case % \. nil \o :yellow \x :red)
                    (filter #{\. \o \x} s))))))

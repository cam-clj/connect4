(ns connect4.core)

(def n-rows 6)
(def n-cols 7)

(def init-state
  {:to-move 0
   :game-over? false
   :board (vec (repeat n-cols []))})

(defn possible-moves
  [board]
  (for [i (range n-cols)
        :when (< (count (nth board i)) n-rows)]
    i))

(defn toggle-player
  [player]
  (case player 0 1 1 0))

(defn get-x-y
  [board x y]
  (get-in board [x y] "."))

(def deltas
  [[[-3 0]  [-2 0]  [-1 0]  [0 0] [1 0]  [2 0]  [3 0]]
   [[0 -3]  [0 -2]  [0 -1]  [0 0] [0 1]  [0 2]  [0 3]]
   [[-3 -3] [-2 -2] [-1 -1] [0 0] [1 1]  [2 2]  [3 3]]
   [[-3 3]  [-2 2]  [-1 1]  [0 0] [1 -1] [2 -2] [3 -3]]])

(defn last-move-won?
  [{:keys [to-move board]} last-x]
  (let [player (toggle-player to-move)
        last-y (dec (count (get board last-x)))]
    (some
     (fn [[a b c d]] (= a b c d player))
     (mapcat
      (fn [delta]
        (let [xs (map (fn [[dx dy]]
                        (get-x-y board
                                 (+ last-x dx)
                                 (+ last-y dy)))
                      delta)]
          (partition 4 1 xs)))
      deltas))))

(defn check-game-over
  [state move]
  (cond
    (last-move-won? state move)
    (assoc state
           :game-over? true
           :winner (toggle-player (:to-move state)))
    (empty? (possible-moves (:board state)))
    (assoc state :game-over? true)
    :else state))

(defn make-move
  [{:keys [to-move board] :as state} move]
  {:pre [(some #{move} (possible-moves board))]}
  (-> state
      (update :to-move toggle-player)
      (update-in [:board move] conj to-move)
      (check-game-over move)))

(defn get-row
  [board r]
  (mapv #(get-x-y board % r) (range n-cols)))

(defn print-board
  [board]
  (println "\n")
  (doseq [r (reverse (range n-rows))]
    (println (get-row board r))))

(defn transpose [xs] (apply map vector xs))

(defn parse-board
  [s]
  (into []
        (comp
         (map (partial remove #{"."}))
         (map (partial map {"0" 0 "1" 1}))
         (map reverse)
         (map vec))
        (transpose (partition n-cols (re-seq #"[01\.]" s)))))

(defprotocol IPlayer
  (next-move [this {:keys [to-move board game-over? winner]}]))

(defrecord RandomPlayer []
  IPlayer
  (next-move [this {:keys [to-move board game-over?]}]
    (when-not game-over?
      (rand-nth (possible-moves board)))))

(defn play-game
  "Play out a game to completion. Return a list of moves and the final
  state in the last position."
  [player1 player2]
  (letfn [(step [state]
            (lazy-seq
             (if (:game-over? state)
               (list state)
               (let [player (case (:to-move state)
                              0 player1
                              1 player2)
                     move (next-move player state)
                     state' (make-move state move)]
                 (cons move (step state'))))))]
    (step init-state)))

(defn play-game-outcome
  "Play out a game to completion. Return the final state."
  [player1 player2]
  (first
   (filter :game-over?
           (iterate (fn [state]
                      (let [player (case (:to-move state)
                                     0 player1
                                     1 player2)
                            move (next-move player state)]
                        (make-move state move)))
                    init-state))))

(comment

  (let [xs (play-game (RandomPlayer.) (RandomPlayer.))
        moves (butlast xs)
        state (last xs)]
    (print-board (:board state))
    (println moves)
    (println "Winner" (:winner state)))

  (frequencies (repeatedly 1000 (comp :winner #(play-game-outcome (RandomPlayer.) (RandomPlayer.)))))



  )

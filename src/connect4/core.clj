(ns connect4.core)

(def n-rows 6)
(def n-cols 7)

(def init-state
  {:to-move    0
   :game-over? false
   :board      (vec (repeat n-cols []))})

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
    (assoc state :game-over? true :winner 2)
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
  [player1 player2 init-state]
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
  [player1 player2 init-state]
  (first
   (filter :game-over?
           (iterate (fn [state]
                      (let [player (case (:to-move state)
                                     0 player1
                                     1 player2)
                            move (next-move player state)]
                        (make-move state move)))
                    init-state))))

(defn monte-carlo-outcomes
  [state n-iters move]
  (let [state (make-move state move)]
    (frequencies
     (repeatedly n-iters
                 (comp :winner
                       #(play-game-outcome (RandomPlayer.) (RandomPlayer.) state))))))

(defrecord MonteCarloPlayer [n-iters]
  IPlayer
  (next-move [this {:keys [to-move board game-over?] :as state}]
    (when-not game-over?
      (let [candidates (possible-moves board)
            outcomes   (zipmap candidates
                               (map (partial monte-carlo-outcomes state n-iters) candidates))]
        (key (first (reverse (sort-by (comp (juxt #(get % to-move) #(get % 2)) val)
                                      outcomes))))))))

(defn bfs
  [state max-depth]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY {:depth 0 :state state}) outcomes []]
    (if (empty? queue)
      outcomes
      (let [{:keys [depth state] :as node} (peek queue)]
        (cond
          (= depth max-depth) (recur (pop queue) (conj outcomes node))
          (:game-over? state) (recur (pop queue) (conj outcomes node))
          :else (recur (into (pop queue)
                             (comp
                              (map (partial make-move state))
                              (map (fn [state] {:depth (inc depth) :state state})))
                             (possible-moves (:board state)))
                       outcomes))))))

(defn score-outcome
  [player {:keys [depth state]}]
  (cond
    (= (:winner state) player) (/ 10 (inc depth))
    (= (:winner state) (toggle-player player)) (/ -100 (inc depth))
    :else 0))

(defn best-move
  [player outcomes]
  (rand-nth (:best-moves
             (reduce (fn [accum [move xs]]
                       (let [score (reduce + 0 (map (partial score-outcome player) xs))]
                         (cond
                           (or (nil? (:best-score accum)) (> score (:best-score accum)))
                           {:best-score score :best-moves [move]}

                           (= score (:best-score accum))
                           (update accum :best-moves conj move)

                           :else accum)))
                     {:best-score nil :best-moves []}
                     outcomes))))

(defrecord LookAheadPlayer [depth]
  IPlayer
  (next-move [this {:keys [to-move board game-over?] :as state}]
    (when-not game-over?
      (let [outcomes (into {}
                           (map (fn [move] [move (bfs (make-move state move) depth)]))
                           (possible-moves board))]
        (best-move to-move outcomes)))))

(comment

  (next-move (LookAheadPlayer. 5) init-state)

  (let [xs (play-game (RandomPlayer.) (LookAheadPlayer. 1) init-state)
        moves (butlast xs)
        state (last xs)]
    (print-board (:board state))
    (println moves)
    (println "Winner" (:winner state)))


  (frequencies (repeatedly 20 (comp :winner #(play-game-outcome (MonteCarloPlayer. 10) (LookAheadPlayer. 3) init-state))))




  )

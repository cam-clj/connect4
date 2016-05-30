(ns connect4.core
  (:require [clojure.string :as str]))

(def n-cols 7)
(def n-rows 6)

;; 5 | 35  36  37  38  39  40  41
;; 4 | 28  29  30  31  32  33  34
;; 3 | 21  22  23  24  25  26  27
;; 2 | 14  15  16  17  18  19  20
;; 1 |  7   8   9  10  11  12  13
;; 0 |  0   1   2   3   4   5   6
;;   +---------------------------
;;      0   1   2   3   4   5   6

(defn xy->ix
  [[x y]]
  (+ x (* y n-cols)))

(defn ix->xy
  [ix]
  [(rem ix n-cols) (quot ix n-cols)])

(defn row
  [y]
  (for [x (range n-cols)] (xy->ix [x y])))

(defn col
  [x]
  (for [y (range n-rows)] (xy->ix [x y])))

(def init-state {:board     (vec (repeat (* n-rows n-cols) nil))
                 :to-move   :red
                 :last-move nil
                 :winner    nil})

(defn next-free-pos
  [board x]
  (first (filter (comp nil? board)
                 (for [y (range n-rows)] (xy->ix [x y])))))

(def valid-move? (comp not nil? next-free-pos))

(defn on-board?
  [[x y]]
  (and (< -1 x n-cols)
       (< -1 y n-rows)))

(defn add-delta
  [x y dx dy]
  [(+ x dx) (+ y dy)])

(defn overlapping-row
  [[x y]]
  (filter on-board? (for [dx (range -3 4)]
                      (add-delta x y dx 0))))

(defn overlapping-col
  [[x y]]
  (filter on-board? (for [dy (range -3 4)]
                      (add-delta x y 0 dy))))

(defn overlapping-l-r-diag
  [[x y]]
  (filter on-board? (for [d (range -3 4)]
                      (add-delta x y d d))))

(defn overlapping-r-l-diag
  [[x y]]
  (filter on-board? (for [d (range -3 4)]
                      (add-delta x y d (- d)))))

(defn winning-candidates
  [ix]
  (let [xy (ix->xy ix)]
    (mapcat (comp (partial partition 4 1)
                  (partial map xy->ix))
            [(overlapping-row xy)
             (overlapping-col xy)
             (overlapping-l-r-diag xy)
             (overlapping-r-l-diag xy)])))

(defn win-for?
  [to-move board positions]
  (every? #(= (board %) to-move) positions))

(defn check-win
  [{:keys [board to-move last-move] :as state}]
  (if (some (partial win-for? to-move board)
            (winning-candidates last-move))
    (assoc state :winner to-move)
    state))

(defn toggle-player
  [x]
  (case x :red :yellow :yellow :red))

(defn make-move
  [{:keys [board to-move] :as state} x]
  {:pre [(valid-move? board x)]}
  (let [ix (next-free-pos board x)]
    (-> state
        (assoc-in [:board ix] to-move)
        (assoc :last-move ix)
        (check-win)
        (update :to-move toggle-player))))

(defn render
  [x]
  (case x
    :red "x"
    :yellow "o"
    "."))

(defn show-state
    [{:keys [board to-move] :as state}]
    (doseq [y (reverse (range n-rows))]
      (println
       (str/join " " (map (comp render board #(xy->ix [% y]))
                          (range n-cols)))))
    (println (str/join " " (range n-cols))))

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
    (println "Game over!" (counter winner) "has won.")
    (let [move (solicit-move state)]
      (recur (make-move state move)))))

(defn new-game
  []
  (game-loop init-state))

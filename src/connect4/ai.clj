(ns connect4.ai
  (:require [clojure.string :as str]
            [connect4.logic :refer [n-cols valid-move? make-move toggle-player]]))

;; (def ^:dynamic *depth* 4)

;; (defn i [state] (select-keys state [:last-move :winner]))

;; (defn build-game-tree
;;   [{:keys [board to-move winner] :as state} depth]
;;   (if (or winner (< depth 0))
;;     [(i state) nil]
;;     [(i state) (for [x (filter (partial valid-move? board) (range n-cols))
;;                     :let [state' (make-move state x)]]
;;                 (build-game-tree state' (dec depth)))]))

;; (defn leaves
;;   [[node children]]
;;   (if (nil? children)
;;     [node]
;;     (mapcat leaves children)))

;; (defn evaluate-move
;;   [state player move]
;;   (let [state' (make-move state move)]
;;     (reduce (fn [score {:keys [winner]}]
;;               (cond
;;                 (nil? winner) score
;;                 (= winner player) (inc score)
;;                 :else             (dec score)))
;;             0
;;             (leaves (build-game-tree state' *depth*)))))

(defn possible-moves
  [{:keys [winner board]}]
  (when-not winner
    (filter (partial valid-move? board) (range n-cols))))

(defn children
  [state]
  (not-empty (map (partial make-move state) (possible-moves state))))

(defn losing?
  [state]
  (some :winner (children state)))

(defn mean
  [xs]
  (when (seq xs)
    (/ (reduce + xs) (count xs))))

(defn evaluate-tree
  [player depth state]
  (let [next-states (children state)]
    (cond
      (some #(= (:winner %) player) next-states)                 1.0
      (some #(= (:winner %) (toggle-player player)) next-states) 0.0
      (<= depth 0)                                               0.5
      :else (mean (map (fn [state]
                         (evaluate-tree player (dec depth) state))
                       next-states)))))


(defn greedy-player
  [{:keys [level] :or {level 2}}]
  (fn [{player :to-move :as state}]
    (let [[_ moves] (reduce (fn [[best-score best-moves] next-state]
                              (let [score (evaluate-tree player level next-state)]
                                (cond
                                  (nil? best-score)    [score [(:last-move next-state)]]
                                  (nil? score)         [best-score best-moves]
                                  (> score best-score) [score [(:last-move next-state)]]
                                  (= score best-score) [score (conj best-moves (:last-move next-state))]
                                  :else                [best-score best-moves])))
                            []
                            (children state))]
      (rand-nth moves))))

(defn basic-player
  "Basic player that makes a winning move when possible, and does not
  make a move that loses immediately if it can be avoided."
  [_]
  (fn [{player :to-move board :board :as state}]
    {:pre [(not (:winner state))]}
    (let [next-states    (children state)
          winning-states (filter :winner next-states)]
      (if (seq winning-states)
        (:last-move (rand-nth winning-states))
        (let [non-losing-states (remove losing? next-states)]
          (if (seq non-losing-states)
            (:last-move (rand-nth non-losing-states))
            (:last-move (rand-nth next-states))))))))

(def computer-move (greedy-player 3))


(comment

  ;; Greedy player not choosing correct move here

  (def s ". . o . . . .
         o . x x . . .
         x x x o o . .
         x o x x o . .
         o x o o x . .
         x o x o x . o")

  (defn parse-board
    [s]
    (into []
          (apply concat
                 (reverse
                  (partition n-cols
                             (map (fn [s]
                                    (case s
                                      "o" :yellow
                                      "x" :red
                                      nil))
                                  (str/split s #"\s+")))))))

  (def state
    {:board (parse-board s) :to-move :red})

  )

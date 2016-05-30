(ns connect4.core
  (:require [clojure.string :as str]
            [connect4.logic :refer [n-rows n-cols
                                    init-state
                                    valid-move? make-move
                                    xy->ix]]
            [connect4.ai :refer [computer-move]])
  (:gen-class))

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

(defn user-move
  [state]
  (show-state state)
  (println (str (render (:to-move state)) " to move: "))
  (let [move (read-move)]
    (if (and move (valid-move? (:board state) move))
      move
      (do
        (println "That is not a valid move!")
        (recur state)))))

(defmulti solicit-move :to-move)

(defmethod solicit-move :red
  [state]
  (computer-move state))

(defmethod solicit-move :yellow
  [state]
  (user-move state))

(defn game-loop
  [state]
  (if-let [winner (:winner state)]
    (do (show-state state)
        (println "Game over!" (render winner) "has won."))
    (let [move (solicit-move state)]
      (recur (make-move state move)))))

(defn yes-no-prompt
  []
  (let [response (read-line)]
    (case (str/lower-case response)
      ("y" "yes") true
      ("n" "no")  false
      (do (println "Please answer yes or no")
          (recur)))))

(defn new-game
  []
  (println "Do you want to play first?")
  (let [player (if (yes-no-prompt) :yellow :red)]
    (game-loop (assoc init-state :to-move player))))

(defn -main
  []
  (loop []
    (new-game)
    (println "Play again?")
    (when (yes-no-prompt)
      (recur))))

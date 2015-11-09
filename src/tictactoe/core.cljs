(ns ^:figwheel-always tictactoe.core
  (:require [reagent.core :as reagent :refer [atom]]
            [cljs.test :refer-macros [deftest is]]))

(enable-console-print!)

(defn new-board [n]
  (vec (repeat n (vec (repeat n "B")))))

(def board-size 3)

(def win-length 3)

(defonce app-state
  (atom {:text "Welcome to tic tac toe"
         :board (new-board board-size)
         :game-status :in-progress}))

(defn computer-move [board]
  (let [remaining-spots (for [i (range board-size)
                              j (range board-size)
                              :when (= (get-in board [j i]) "B")]
                          [j i])
        move (when (seq remaining-spots)
               (rand-nth remaining-spots))]
    (if move
      (assoc-in board move "C")
      board)))

(deftest computer-move-test
  (is (= [["C"]]
         (computer-move [["B"]])))
  (is (= [["P"]]
         (computer-move [["P"]]))))

(defn straight [owner board [x y] [dx dy] n]
  (every? true?
         (for [i (range n)]
           (= (get-in board [(+ (* dx i) x)
                             (+ (* dy i) y)])
              owner))))

(defn win? [owner board n]
  (some true?
        (for [i (range board-size)
              j (range board-size)
              dir [[1 0] [0 1] [1 1] [1 -1]]]
          (straight owner board [i j] dir n))))

(deftest win?-test
  (is (win? "P" [["P"]] 1))
  (is (not (win? "P" [["P"]] 2)))
  (is (win? "P" [["C" "P"]
                 ["P" "C"]] 2)))

(defn full? [board]
  (every? #{"P" "C"} (apply concat board)))

(deftest full?-test
  (is (not (full? [["P" "B"]])))
  (is (full? [["P" "C"]])))

(defn game-status [board]
  (cond
    (win? "P" board win-length) :player-victory
    (win? "C" board win-length) :computer-victory
    (full? board) :draw
    :else :in-progress))

(defn update-status [state]
  (assoc state :game-status (game-status (:board state))))

(defn check-game-status [state]
  (-> state
      (update-in [:board] computer-move)
      (update-status)))

(defn blank [i j]
  [:rect
   {:width 0.9
    :height 0.9
    :fill "grey"
    :x (+ 0.05 i)
    :y (+ 0.05 j)
    :on-click
    (fn blank-click [e]
      (when (= (:game-status @app-state) :in-progress)
        (swap! app-state assoc-in [:board j i] "P")
        (if (win? "P" (:board @app-state) win-length)
          (swap! app-state assoc :game-status :player-victory)
          (swap! app-state check-game-status))))}])

(defn circle [i j]
  [:circle
   {:r 0.35
    :stroke "green"
    :stroke-width 0.12
    :fill "none"
    :cx (+ 0.5 i)
    :cy (+ 0.5 j)}])

(defn cross [i j]
  [:g {:stroke "darkred"
       :stroke-width 0.4
       :stroke-linecap "round"
       :transform
       (str "translate(" (+ 0.5 i) "," (+ 0.5 j) ") "
            "scale(0.3)")}
   [:line {:x1 -1 :y1 -1 :x2 1 :y2 1}]
   [:line {:x1 1 :y1 -1 :x2 -1 :y2 1}]])

(defn tictactoe []
  [:center
   [:h1 (:text @app-state)]
   [:h2
    (case (:game-status @app-state)
      :player-victory "You won! "
      :computer-victory "Computer wins. "
      :draw "Draw. "
      "")
    [:button
     {:on-click
      (fn new-game-click [e]
        (swap! app-state assoc
               :board (new-board board-size)
               :game-status :in-progress))}
     "New Game"]]
   (into
    [:svg
     {:view-box (str "0 0 " board-size " " board-size)
      :width 500
      :height 500}]
    (for [i (range board-size)
          j (range board-size)]
      (case (get-in @app-state [:board j i])
        "B" [blank i j]
        "P" [circle i j]
        "C" [cross i j])))])

(reagent/render-component [tictactoe]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  (prn (:board @app-state)))

(ns fn-in-the-dark.core
  (:require [quil.core :as q]
            [quil.middleware :as m])
  (:import java.awt.event.KeyEvent))

;; Config
(def game-state (atom {}))
(def world {:width 20 :height 20})
(def fps 30)
(def enemy-frames 10)
(def number-of-barriers 30)

;; Positions
(def player (atom []))
(def enemy (atom []))
(def bling (atom []))
(def walls (atom []))
(def barriers (atom []))
(def goal (atom []))
(def corner-positions [[0 0] [0 19] [19 0] [19 19]])
(def messages (atom []))

(defn init-game-state
  []
  (reset! game-state {:bling? false
                      :instructions? true
                      :shutter-closed? true
                      :win? false
                      :lose? false
                      :frame 0}))

(defn random-position
  ([]
   [(rand-int (world :width)) (rand-int (world :height))])
  ([min max]
   [(rand-nth (range min max)) (rand-nth (range min max))])
  ([minx maxx miny maxy]
   [(rand-nth (range minx maxx)) (rand-nth (range miny maxy))]))

(defn surrounding-positions
  [[x y]]
  [[(+ x 1) y] [x (+ y 1)] [(- x 1) y] [x (- y 1)]])

(defn populate-world
  []
  (let [walls-pos (concat (map (fn [i] [0 i]) (range (:width world)))
                          (map (fn [i] [19 i]) (range (:width world)))
                          (map (fn [i] [i 0]) (range (:height world)))
                          (map (fn [i] [i 19]) (range (:height world))))
        goal-pos (->> walls-pos (remove #((set corner-positions) %)) rand-nth)
        bling-pos (random-position 1 (- (world :width) 1))
        enemy-pos (random-position 1 (- (world :width) 1))
        barriers-pos (->> (repeatedly number-of-barriers #(random-position 1 18))
                          (remove #((set (conj (surrounding-positions goal-pos) @player bling-pos enemy-pos)) %)))]
    (reset! walls walls-pos)
    (reset! goal goal-pos)
    (reset! enemy enemy-pos)
    (reset! player [10 10])
    ;; TODO avoid collision with player and walls
    (reset! bling bling-pos)
    (reset! barriers barriers-pos)))

(defn setup []
  (q/frame-rate fps)
  (init-game-state)
  (populate-world)
  (reset! messages [])
  (q/background 0))


(defn collide-with-wall?
  [pos]
  ((set (concat @walls @barriers)) pos))

(defn move-player
  [new-pos]
  ;; Player only moves when shutter closed and next space isn't a wall
  (cond
    (and (:shutter-closed? @game-state)
         (not (collide-with-wall? new-pos))) (do (reset! player new-pos)
                                                 (reset! messages []))
    (and (:shutter-closed? @game-state)
         (collide-with-wall? new-pos)) (swap! messages conj [:ouch @player])
    :else (swap! messages conj [:stuck @player])))

(defn controls
  [_ event]
  (prn "CONTROL" @game-state)
  (let [[x y] @player]
    (case (:key event)
      :up (move-player [x (dec y)])
      :down (move-player [x (inc y)])
      :left (move-player [(dec x) y])
      :right (move-player [(inc x) y])
      :space (do (swap! game-state update :shutter-closed? not)
                 (swap! game-state assoc :instructions? false)
                 (reset! messages []))
      :r (setup)
      nil)
    (q/redraw)))

(defn bling-taken
  []
  (swap! game-state assoc :bling? true)
  (swap! messages conj [:bling @bling])
  (reset! bling [])
  (swap! walls (fn [w] (remove #(= @goal %) w))))

(defn remove-wall-pos
  [positions]
  (->> positions
       (remove #(collide-with-wall? %))
       (not-empty)))

(defn move-enemy
  []
  ;; Enemy moves towards player when light is on
  (let [[ex ey] @enemy
        [px py] @player
        neighbour-pos (remove-wall-pos [[ex (dec ey)][ex (inc ey)][(dec ex) ey][(inc ex) ey]])
        tracking-pos (remove-wall-pos (filter identity
                                       (conj []
                                             (when (not (zero? (compare py ey)))
                                               [ex (+ ey (compare py ey))])
                                             (when (not (zero? (compare px ex)))
                                               [(+ ex (compare px ex)) ey]))))]
    (reset! enemy (-> (if (:shutter-closed? @game-state)
                        neighbour-pos
                        (or tracking-pos neighbour-pos))
                      (rand-nth)))))

(defn update-state [_]
  (swap! game-state update :frame inc)
  (cond
    ;;enemy move
    (and (false? (:instructions? @game-state))
         (zero? (rem (:frame @game-state) enemy-frames))) (move-enemy)
    ;; bling taken
    (= @player @bling) (bling-taken)
    
    ;; Win
    (= @player @goal) (swap! game-state assoc :win? true)

    ;; Lose
    (= @player @enemy) (swap! game-state assoc :lose? true)
    
    :default nil))

(defn filter-message-positions
  [message]
  (->> (filter #(= message (first %)) @messages)
       (map second)))

;; DRAWING

(defn draw-width
  []
  (/ (q/width) (world :width)))

(defn draw-height
  []
  (/ (q/height) (world :height)))

(defn draw-goal
  []
  (let [[x y] @goal]
    (q/no-stroke)
    (q/fill 0 255 0)
    (q/rect (* (draw-width) x) (* (draw-height) y) (draw-width) (draw-height))))

(defn draw-player
  []
  (let [[x y] @player]
    (q/fill 0 0 255)
    (q/stroke 0 0 0)
    (q/rect (* (draw-width) x) (* (draw-height) y) (draw-width) (draw-height) 10)
    (q/stroke 0 0 0)
    (q/fill 255 203 164)
    (q/ellipse-mode :corner)
    (q/ellipse (+ (* (draw-width) 0.25) (* (draw-width) x)) (+ (* (draw-height) 0.25) (* (draw-height) y))
               (/ (draw-width) 2) (/ (draw-height) 2))))

(defn draw-enemy
  []
  (let [[x y] @enemy]
    (q/fill 201 71 245)
    (q/stroke 0 0 0)
    (q/rect (* (draw-width) x) (* (draw-height) y) (draw-width) (draw-height))
    (q/stroke 0 0 0)
    (q/fill 0 0 0)
    ;; Eyes?
    (q/triangle (+ (* (draw-width) 0.125) (* (draw-width) x)) (+ (* (draw-height) 0.9) (* (draw-height) y))
                (+ (* (draw-width) 0.375) (* (draw-width) x)) (+ (* (draw-height) 0.9) (* (draw-height) y))
                (+ (* (draw-width) 0.25) (* (draw-width) x)) (+ (* (draw-height) 0.75) (* (draw-height) y)))
    (q/triangle (+ (* (draw-width) 0.675) (* (draw-width) x)) (+ (* (draw-height) 0.9) (* (draw-height) y))
                (+ (* (draw-width) 0.925) (* (draw-width) x)) (+ (* (draw-height) 0.9) (* (draw-height) y))
                (+ (* (draw-width) 0.825) (* (draw-width) x)) (+ (* (draw-height) 0.75) (* (draw-height) y)))))

(defn draw-walls
  []
  (doseq [[x y] @walls]
    (q/fill 94 93 92)
    (q/stroke 0 0 0)
    (q/rect (* (draw-width) x) (* (draw-height) y) (draw-width) (draw-height))))

(defn draw-barriers
  []
  (doseq [[x y] @barriers]
    (q/fill 94 93 92)
    (q/stroke 0 0 0)
    (q/rect (* (draw-width) x) (* (draw-height) y) (draw-width) (draw-height))))

(defn draw-bling
  []
  (let [[x y] @bling]
    (q/ellipse-mode :corner)
    (q/fill 255 255 0)
    (q/stroke 0 0 0)
    (q/ellipse (+ (* (draw-width) 0.25) (* (draw-width) x))
               (+ (* (draw-height) 0.25) (* (draw-height) y))
               (/ (draw-width) 2)
               (/ (draw-height) 2))))

(defn draw-win-screen
  []
  (q/background 255 255 255)
  (q/fill 0 0 0)
  (q/text-align :center)
  (q/text "YOU WIN!" (* (draw-width) 10) (* (draw-height) 10))
  (q/text-align :center)
  (q/text "[Press r to restart]" (* (draw-width) 10) (* (draw-height) 11)))

(defn draw-lose-screen
  []
  (q/background 255 255 255)
  (q/fill 0 0 0)
  (q/text-align :center)
  (q/text "GAME OVER!" (* (draw-width) 10) (* (draw-height) 10))
  (q/text-align :center)
  (q/text "[Press r to restart]" (* (draw-width) 10) (* (draw-height) 11)))

(defn draw-shutter
  []
  (let [bling (first (filter-message-positions :bling))
        ouch (first (filter-message-positions :ouch))
        {:keys [instructions?]} @game-state]
    (q/fill 0 0 0)
    (q/rect 10 10 800 800)

    (when instructions?
      (q/fill 255 255 255)
      (q/text-align :center)
      (q/text-size 30)
      (q/text "FNBLING IN THE DARK" (* (draw-width) 10) (* (draw-height) 3))  
      (q/text-size 15)
      (q/text "Avoid the monster, find the bling and escape the maze.\n The sight of the monster petrifies you and draws it to you.\n Use the light to help you navigate but you can only move in the dark." (* (draw-width) 10) (* (draw-height) 6))
      (q/text "[Press space to turn the light on]" (* (draw-width) 10) (* (draw-height) 11)))

    (when-let [[x y] bling]
      (q/fill 255 255 255)
      (q/text-size 15)
      (q/text-align :center :top)
      (q/text "Bling!" (* (draw-width) (+ x 0.5)) (* (draw-height) y)))

    (when-let [[x y] ouch]
      (q/fill 255 255 255)
      (q/text-size 15)
      (q/text-align :center :top)
      (q/text "Ouch!" (* (draw-width) (+ x 0.5)) (* (draw-height) y)))))

(defn draw-play-screen
  []
  (let [{:keys [bling? shutter-closed?]} @game-state
        stuck (first (filter-message-positions :stuck))]
    (q/background 192 192 192)

    ;; Enemy
    (draw-enemy)

    ;; bling
    (when (not-empty @bling)
      (draw-bling))

    ;; player
    (draw-player)

    ;; barriers
    (draw-barriers)
    
    ;; shutter
    (when shutter-closed?
      (draw-shutter))

    ;; goal
    (when bling?
      (draw-goal))
    
    ;; walls
    (draw-walls)

    (when-let [[x y] stuck]
      (q/fill 0 0 0)
      (q/text-size 15)
      (q/text-align :center :top)
      (q/text "I'm stuck!" (* (draw-width) (+ x 0.5)) (* (draw-height) (- y 0.5))))))

(defn draw [_]
  (let [{:keys [win? lose?]} @game-state]
    (cond
      win? (draw-win-screen)
      lose? (draw-lose-screen)
      :else (draw-play-screen))))

(defn -main [& args]
  (q/sketch
   :title "fnbling in the dark"
   :size [800 800]
   :setup setup
   :update update-state
   :draw draw
   :features [:keep-on-top]
   :key-pressed controls
   :middleware [m/fun-mode]))

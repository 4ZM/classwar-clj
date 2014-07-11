(ns classwar
  (:require [clojure.string :as str]))

(defn setup-game []
  "Create initial game state"
  {:day 1

   :au 10 ;; activist units
   :mu 100 ;;  monitary units

   :revolutionary-potential 0.01 ;; %

   :facist-activity         0.05 ;; %
   :capitalist-activity     0.10 ;; %
   :police-repression       0.00 ;; %
   :political-climate       0.00 ;; -1 to 1 (left / right)

   :status :running})


(defn show-game-overview [g]
  (println (format "Game overview, day %d" (g :day))))


(defn available-options [g]
  [{:id :keep-playing :desc "Keep playing" :duration 1 :effort 1}
   {:id :surender :desc "Give up" :duration 0 :effort 0}])


(defn- format-menu-option [i opt]
  (format "%d. %s [%d AU | %d d]" i (opt :desc) (opt :effort) (opt :duration)))

(defn user-input []
  (let [str-opt (read-line)]
    (Integer/parseInt str-opt)))

(defn vec-input [v]
  (let [va (atom v)]
    (fn []
      (let [x (first @va)]
        (swap! va rest)
        x))))

(defn action-menu [opts input]
  (println "Action Menu")
  (println (str/join "\n" (map-indexed format-menu-option opts)))
  (print "Select: ")
  (let [sel (input)]
    (println sel)
    (opts sel)))

(defn get-actions [g input]
  (let [opts (available-options g)]
    [(action-menu opts input)]))

(defn create-events [g] [])

(defn tic [g actions]
  (create-events g)
  (let [new-game (atom g)]
    ;; Advance to next day
    (reset! new-game (update-in @new-game[:day] inc))

    ;; Give up?
    (if (some #{:surender} (map :id actions))
      (reset! new-game (assoc-in @new-game [:status] :game-over)))

    ;; Return new game state
    @new-game))


(defn play [input]
  (loop [g  (setup-game)]
    (show-game-overview g)
    (let [actions (get-actions g input)
          new-game (tic g actions)]
      (println "Debug" new-game)
      (if (= (new-game :status) :running) (recur new-game))))
  (println "Game Over"))

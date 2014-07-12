(ns classwar
  (:require [classwar.actions :as cwa]
            [classwar.events :as cwe]
            [clojure.string :as str]))

(defn setup-game []
  "Create initial game state"
  {:day 1

   :activists                  5 ;; Number of
   :revolutionary-potential 0.00 ;; %
   :organized-workforce     0.00 ;; %
   :money                   1000 ;; $$

   :facist-activity         0.05 ;; %
   :capitalist-activity     0.10 ;; %
   :police-repression       0.00 ;; %

   :political-climate       0.00 ;; -1 to 1 (left / right)

   :institutions []              ;; Support groups and structures

   :police-noticed         false ;; Police knows about the movement

   :status :running})


(defn show-game-overview [g]
  (println (format "Game overview, day %d" (g :day)))
  (println (format "  Activists: %d  Workforce: %2.2f  Revolution: %2.2f"
                   (g :activists)
                   (g :organized-workforce)
                   (g :revolutionary-potential)))
  (println (format "  Facists: %2.2f  Capitalists: %2.2f  Police: %2.2f  Climate: %2.2f"
                   (g :facist-activity)
                   (g :capitalist-activity)
                   (g :police-repression)
                   (g :political-climate))))

(defn available-options [g available-activists]
  (let [actions [cwa/nop
                 cwa/surender
                 cwa/demo
                 cwa/handout-flyers
                 cwa/posters
                 cwa/stickers
                 cwa/online-campaign
                 cwa/party
                 cwa/start-antifa-group]]
    (filter #(< (% :effort) available-activists) actions)))

(defn- format-menu-option [i opt]
  (format "  %d. %s [%d A]" i (opt :desc) (opt :effort)))

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
  (print "  Select: ")
  (let [sel (input)]
    (println sel)
    (nth opts sel)))

(defn get-actions [g input]
  (let [available-activists (g :activists)
        opts (available-options g available-activists)]
    [(action-menu opts input)]))

(defn tic [g actions events]

  (let [new-game (atom g)]

    ;; Do all the actions
    (doall
     (for [a actions]
       (reset! new-game ((a :action) @new-game a actions events))))

    ;; Let institutions act
    (doall
     (for [i (@new-game :institutions)]
       (reset! new-game ((i :action) @new-game i actions events))))

    ;; Do game logic
    (doall
     (for [e events]
       (do (println "EVENT! " (e :desc))
           (reset! new-game ((e :action) @new-game)))))

    ;; Advance to next day
    (reset! new-game (update-in @new-game [:day] inc))

    ;; Return new game state
    @new-game))

(defn play [input]
  (loop [g  (setup-game)]
    (show-game-overview g)
    (let [actions (get-actions g input)
          events (cwe/current-events g actions)
          new-game (tic g actions events)]
      (if (= (new-game :status) :running) (recur new-game))))
  (println "Game Over"))

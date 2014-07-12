(ns classwar
  (:require [classwar.actions :as cwa]
            [clojure.string :as str]))

(defn setup-game []
  "Create initial game state"
  {:day 1

   :mu 1000                      ;;  monitary units

   :activists                  5 ;; Number of
   :revolutionary-potential 0.00 ;; %
   :organized-workforce     0.00 ;; %

   :facist-activity         0.05 ;; %
   :capitalist-activity     0.10 ;; %
   :police-repression       0.00 ;; %

   :political-climate       0.00 ;; -1 to 1 (left / right)

   :events []
   :institutions []              ;; Support groups and structures

   :status :running})


(defn show-game-overview [g]
  (println (format "Game overview, day %d" (g :day)))
  (println (format "  Activists: %d  Workforce: %2.2f  Revolution: %2.2f"
                   (g :activists)
                   (g :organized-workforce)
                   (g :revolutionary-potential)))
  (println (format "  Facists: %2.2f  Capitalists: %2.2f  Police: %2.2f"
                   (g :facist-activity)
                   (g :capitalist-activity)
                   (g :police-repression))))

(defn available-options [activists]
  (let [actions [cwa/nop
                 cwa/surender
                 cwa/demo
                 cwa/online-campaign
                 cwa/party
                 cwa/start-antifa-group]]
    (filter #(< (% :effort) activists) actions)))

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
  (let [activists (g :activists)
        opts (available-options activists)]
    [(action-menu opts input)]))

(defn create-events [g a] [])

(defn tic [g actions]

  (let [new-game (atom g)]

    ;; Do all the actions
    (doall
     (for [a actions]
       (reset! new-game ((a :action) @new-game a))))

    ;; Let institutions act
    (doall
     (for [i (@new-game :institutions)]
       (reset! new-game ((i :action) @new-game i))))

    ;; Advance to next day
    (reset! new-game (update-in @new-game [:day] inc))

    ;; Return new game state
    @new-game))

(defn play [input]
  (loop [g  (setup-game)]
    (show-game-overview g)
    (let [actions (get-actions g input)
          events (create-events g actions)
          new-game (tic g actions)]
      ;;(println "Debug" new-game)
      (if (= (new-game :status) :running) (recur new-game))))
  (println "Game Over"))

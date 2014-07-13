(ns classwar
  (:require [classwar.actions :as cwa]
            [classwar.events :as cwe]
            [clojure.string :as str]))

(defn setup-game []
  "Create initial game state"
  {:day 1

   :activists                  5  ;; Number of
   :revolutionary-potential 0.00  ;; %
   :organized-workforce     0.00  ;; %
   :money                   1000  ;; $$

   :fascists {:activity     0.05  ;; %
             :power         0.01} ;; %

   :capitalists {:activity  0.10  ;; %
                 :power     0.50} ;; %

   :police-repression       0.00  ;; %

   :political-climate       0.00  ;; -1 to 1 (left / right)

   :institutions              []  ;; Support groups and structures

   :police-noticed         false  ;; Police knows about the movement

   :status :running})


(defn show-game-overview [g]
  (println (format "Game overview, day %d" (g :day)))
  (println (format "  Activists: %d  Workforce: %2.2f  Revolution: %2.2f"
                   (g :activists)
                   (g :organized-workforce)
                   (g :revolutionary-potential)))
  (println (format "  Fascists>    Power: %2.2f  Activity: %2.2f"
                   (-> g :fascists :power)
                   (-> g :fascists :activity)))
  (println (format "  Capitalists> Power: %2.2f  Activity: %2.2f"
                   (-> g :capitalists :power)
                   (-> g :capitalists :activity)))
  (println (format "  Police Repression: %2.2f  Political Climate: %2.2f"
                   (g :police-repression)
                   (g :political-climate))))

(defn available-options [g available-activists]
  (let [actions [cwa/nop
                 cwa/surender
                 (cwa/create-demo :antifa 5)
                 (cwa/create-demo :anticap 5)
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

(defn tic [game actions events]

  (let [g (atom game)]

    ;; Do all the actions
    (doall
     (for [a actions]
       (reset! g ((a :action) @g a actions events))))

    ;; Let institutions act
    (doall
     (for [i (@g :institutions)]
       (reset! g ((i :action) @g i actions events))))

    ;; Let events play out
    (doall
     (for [e events]
       (do (println "EVENT! " (e :desc))
           (reset! g ((e :action) @g)))))

    ;; Do game logic
    (reset! g (update-in @g [:fascists :power] + (-> @g :fascists :activity)))
    (reset! g (update-in @g [:capitalists :power] + (-> @g :capitalists :activity)))

    ;; Advance to next day
    (reset! g (update-in @g [:day] inc))

    ;; Return new game state
    @g))

(defn play [input]
  (loop [g  (setup-game)]
    (show-game-overview g)
    (let [actions (get-actions g input)
          events (cwe/current-events g actions)
          g (tic g actions events)]
      (if (= (g :status) :running) (recur g))))
  (println "Game Over"))

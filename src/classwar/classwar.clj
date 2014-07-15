(ns classwar
  (:require [classwar.actions :as cwa]
            [classwar.events :as cwe]
            [classwar.ui :as cwui]
            [lonocloud.synthread :as ->]))

(defn activist-capacity [g]
  "Max number of activists that can be organized"
  ;; Inc with better infrastructure
  10)

(defn setup-game []
  "Create initial game state"
  {:day 1

   :activists                  5  ;; Number of
   :prospects                  0  ;; Possible recruits

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

(defn get-actions [g input]
  (let [available-activists (g :activists)
        opts (available-options g available-activists)]
    [(cwui/action-menu opts input)]))

(defn execute-actions [game actions events]
  (let [action-fns (map (fn [a] (fn [g] ((a :action) g a actions events)))
                        actions)]
    ((apply comp action-fns) game)))

(defn institution-updates [game actions events]
  (let [institution-fns (map (fn [i] (fn [g] ((i :action) g i actions events)))
                             (game :institutions))]
    ((apply comp institution-fns) game)))

(defn execute-events [game actions events]
  (let [event-fns (map :action events)]
    ((apply comp event-fns) game)))

(defn max-recruitment [{activists :activists prospects :prospects :as g}]
  (let [space (- (activist-capacity g) activists)]
    (min space prospects)))

(defn recruit-activists [g]
  (-> g
      (->/as (-> max-recruitment new-activists)
             (update-in [:activists] + new-activists)) ; update will be in clj 1.7
      (assoc :prospects 0)))

(defn update-opponent-power [g]
  (-> g
      (update-in [:fascists :power] + (-> g :fascists :activity))
      (update-in [:capitalists :power] + (-> g :capitalists :activity))))

(defn update-game-status [g]
  (-> g
      (cond->
       (>= (-> g :fascists :power) 1.0)
       (assoc :status :fascists-win))
      (cond->
       (>= (-> g :capitalists :power) 1.0)
       (assoc :status :capitalists-win))))

(defn tic [game actions events]
  (-> game
      (execute-actions actions events)
      (institution-updates actions events)
      (execute-events actions events)
      recruit-activists
      update-opponent-power
      update-game-status
      (update-in [:day] inc)))

(defn game-over [g]
  (println "GAME OVER")
  (case (g :status)
    :surender (println " > You give up")
    :fascists-win (println " > Fascists Win - You lose")
    :capitalists-win (println " > Capitalists Win - You lose")))

(defn play [input]
  (loop [g  (setup-game)]
    (cwui/show-game-overview g)
    (let [actions (get-actions g input)
          events (cwe/current-events g actions)
          g (tic g actions events)]
      (if (= (g :status) :running)
        (recur g)
        (game-over g)))))

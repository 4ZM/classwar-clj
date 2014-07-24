(ns classwar.state
  (:require [lonocloud.synthread :as ->]))

(def initial-game-state
  "Create initial game state"
  {:day 0

   :activists                  5  ;; Number of
   :recruitable                0  ;; Possible recruits

   :revolutionary-potential 0.00  ;; %
   :organized-workforce     0.00  ;; %
   :money                   1000  ;; $$

   :fascists
   {:activity               0.05  ;; %
    :power                  0.01} ;; %

   :capitalists
   {:activity               0.10  ;; %
    :power                  0.50} ;; %

   :police-repression       0.00  ;; %

   :political-climate       0.50  ;; % red (0 = deep blue)

   :police-noticed         false  ;; Police knows about the movement

   :institutions             #{}  ;; Support groups and structures

   :operations               #{}  ;; Running operations
   :digest                   #{}  ;; Messages for the day

   :status :running})


(defn has-institution? [id {institutions :institutions}]
  (some #{id} (map :id institutions)))

(defn activist-capacity [{institutions :institutions}]
  "Max number of activists that can be organized"
  (max 10 (reduce + (keep :activist-capacity institutions))))

(defn running-events [game]
  (filter #(= (% :type) :event) (game :operations)))
(defn running-actions [game]
  (filter #(= (% :type) :action) (game :operations)))

(defn max-recruitment [{activists :activists recruitable :recruitable :as g}]
  (let [space (- (activist-capacity g) activists)]
    (min space recruitable)))

(def ACTIVIST_DAILY_DONATION 5)

(defn daily-donations [g]
  (let [free-activists (g :activists)
        bound-activists (keep :activists (g :insitutions))
        all-activists (reduce + free-activists bound-activists)]
    (* all-activists ACTIVIST_DAILY_DONATION)))
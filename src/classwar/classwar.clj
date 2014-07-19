(ns classwar
  (:require [classwar.actions :as cwa]
            [classwar.events :as cwe]
            [classwar.ui :as cwui]
            [lonocloud.synthread :as ->]))

(defn activist-capacity [g]
  "Max number of activists that can be organized"
  (cond
   (some #{:comunity-center} (map :id (g :institutions))) 30
   :default 10))

(defn setup-game []
  "Create initial game state"
  {:day 0

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

   :institutions             #{}  ;; Support groups and structures

   :police-noticed         false  ;; Police knows about the movement

   :actions                  #{}  ;; Running actions to be executed
   :events                   #{}  ;; Running events to be executed

   :status :running})

(defn has-institution? [id {institutions :institutions}]
  (some #{id} (map :id institutions)))

(defn revolution-available? [g]
  (and (has-institution? :union g)
       (has-institution? :comunity-center g)
       (has-institution? :antifa-group g)))

(defn available-options [g available-activists available-money]
  (let [actions [cwa/nop
                 cwa/surender
                 (cwa/create-demo :antifa 5)
                 (cwa/create-demo :anticap 5)
                 cwa/handout-flyers
                 cwa/posters
                 cwa/stickers
                 cwa/online-campaign
                 cwa/party
                 cwa/reclaim-party]
        activist-filter (partial filter #(< (cwa/effort %) available-activists))
        cost-filter (partial filter #(< (cwa/cost %) available-money))]
    (-> actions
        (cond-> (not (has-institution? :union g))
                (conj cwa/start-union))
        (cond-> (not (has-institution? :comunity-center g))
                (conj cwa/start-comunity-center))
        (cond-> (not (has-institution? :antifa-group g))
                (conj cwa/start-antifa-group))
        (cond-> (revolution-available? g)
                (conj cwa/revolution))
        activist-filter
        cost-filter)))

(defn get-actions [g input]
  (let [available-activists (g :activists)
        opts (available-options g available-activists (g :money))]
    [(cwui/action-menu opts input)]))

(defn- execute-ops [game op-tag]
  (let [action-fns (map (fn [a] (fn [g] ((a :action) g a)))
                        (game :actions))]
    ((apply comp action-fns) game)))

(defn execute-actions [game] (execute-ops game :actions))
(defn institution-updates [game] (execute-ops game :institutions))
(defn execute-events [game] (execute-ops game :events))


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
      (update-in [:fascists :power] + (* 0.1 (-> g :fascists :activity)))
      (update-in [:capitalists :power] + (* 0.1 (-> g :capitalists :activity)))))

(defn update-game-status [g]
  (-> g
      (cond->
       (some #{:revolution} (map :id (g :actions)))
       (assoc :status :revolution))
      (cond->
       (>= (-> g :fascists :power) 1.0)
       (assoc :status :fascists-win))
      (cond->
       (>= (-> g :capitalists :power) 1.0)
       (assoc :status :capitalists-win))))

(defn collect-money [g]
  (let [free-activists (g :activists)
        bound-activists (keep :activists (g :insitutions))
        all-activists (reduce + free-activists bound-activists)
        collected-money (* all-activists 5)]
    (update-in g [:money] + collected-money)))

(defn tic [game actions events]
  (-> game
      (update-in [:actions] into actions)
      (update-in [:events] conj events)

      execute-actions
      institution-updates
      execute-events

      collect-money
      recruit-activists
      update-opponent-power
      update-game-status
      (update-in [:day] inc)))

(defn game-over [g]
  (println "GAME OVER")
  (case (g :status)
    :surender (println " > You give up")
    :revolution (println " > Revolution! You win!")
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

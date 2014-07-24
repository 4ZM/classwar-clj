(ns classwar.classwar
  (:require [classwar.op :as cwo]
            [classwar.state :as cws]
            [classwar.actions :as cwa]
            [classwar.events :as cwe]
            [classwar.ui :as cwui]
            [lonocloud.synthread :as ->]))


(defn revolution-available? [g]
  (and (cws/has-institution? :union g)
       (cws/has-institution? :comunity-center g)
       (cws/has-institution? :antifa-group g)))

(defn available-options [g available-activists available-money]
  (let [actions [cwa/nop-action
                 cwa/surender
                 cwa/antifa-demo
                 cwa/anticap-demo
                 cwa/handout-flyers
                 cwa/posters
                 cwa/stickers
                 cwa/online-campaign
                 cwa/party
                 cwa/reclaim-party]
        activist-filter (partial filter #(< (cwo/effort %) available-activists))
        cost-filter (partial filter #(< (cwo/cost %) available-money))]
    (-> actions
        (cond-> (not (cws/has-institution? :union g))
                (conj cwa/start-union))
        (cond-> (not (cws/has-institution? :comunity-center g))
                (conj cwa/start-comunity-center))
        (cond-> (not (cws/has-institution? :occupied-building g))
                (conj cwa/occupy-building))
        (cond-> (not (cws/has-institution? :antifa-group g))
                (conj cwa/start-antifa-group))
        (cond-> (revolution-available? g)
                (conj cwa/revolution))
        activist-filter
        cost-filter)))

(defn get-actions [g input]
  (let [available-activists (g :activists)
        opts (available-options g available-activists (g :money))]
    [(cwui/action-menu opts input)]))

(defn get-events [g input]
  "Debuging to get events from user"
  (let [events [cwe/nop-event
                cwe/fascist-flyers
                cwe/fascist-burn-comunity-center
                cwe/police-evicts-occupied-building
                cwe/capitalist-ad-campaign
                cwe/police-notices]
        ;; Replace probabilities with acctual values
        events (map #(assoc % :probability ((% :probability) g)) events)]
    [(cwui/event-menu events input)]))

(defn- execute-ops [game ops]
  (let [action-fns (map (fn [a] (fn [g] ((a :op) g a))) ops)]
    ((apply comp action-fns) game)))

(defn execute-actions [game] (execute-ops game (cws/running-actions game)))
(defn institution-updates [game] (execute-ops game (game :institutions)))
(defn execute-events [game] (execute-ops game (cws/running-events game)))

(defn recruit-activists [g]
  (-> g
      ;; Recruit as many as possible
      (->/as
       (-> cws/max-recruitment new-activists)
       (update-in [:activists] + new-activists))

      ;; Remove recruitable by half if they aren't recruited
      (update-in [:recruitable] quot 2)))

(defn update-opponent-power [g]
  (-> g
      (update-in [:fascists :power] + (* 0.1 (-> g :fascists :activity)))
      (update-in [:capitalists :power] + (* 0.1 (-> g :capitalists :activity)))))

(defn update-game-status [g]
  (-> g
      (cond->
       (some #{:revolution} (map :id (g :operations)))
       (assoc :status :revolution))
      (cond->
       (>= (-> g :fascists :power) 1.0)
       (assoc :status :fascists-win))
      (cond->
       (>= (-> g :capitalists :power) 1.0)
       (assoc :status :capitalists-win))))

(defn collect-money [g]
  (update-in g [:money] + (cws/daily-donations g)))

(defn tic [game actions events]
  (-> game
      (assoc :digest #{})
      (update-in [:operations] into actions)
      (update-in [:operations] into events)

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
  (loop [g  cws/initial-game-state last-g  cws/initial-game-state]
    (cwui/show-game-overview g last-g)
    (let [actions (get-actions g input)
          events (get-events g input) ;; For debuging events
          new-game (tic g actions events)]
      (if (= (new-game :status) :running)
        (recur new-game g)
        (game-over new-game)))))

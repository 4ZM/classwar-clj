;; Copyright (C) 2014 Anders Sundman <anders@4zm.org>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(ns classwar.classwar
  (:gen-class)
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
                 cwa/anticap-flyers
                 cwa/antifa-flyers
                 cwa/posters
                 cwa/stickers
                 cwa/online-antifa-campaign
                 cwa/party
                 cwa/reclaim-party]
        activist-filter (partial filter #(< (cwo/effort %) available-activists))
        cost-filter (partial filter #(< (cwo/cost %) available-money))]
    (-> actions
        (cond-> (some (comp #{:fascist-posters} :id) (cws/running-events g))
                (conj cwa/tear-down-fascist-propaganda))
        (cond-> (some (comp #{:capitalist-ad-campaign} :id) (cws/running-events g))
                (conj cwa/adbusting))
        (cond-> (cwo/last-day? (cws/running-op g :fascist-demo))
                (conj cwa/counter-fascist-demo))
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

(defn rand-include-event? [g {pfn :probability}]
  (< (rand) (pfn g)))

(defn get-events [g input]
  "Debuging to get events from user"
  (let [events [cwe/nop-event
                cwe/fascist-flyers
                cwe/fascist-burn-comunity-center
                cwe/fascist-posters
                cwe/police-evicts-occupied-building
                cwe/capitalist-ad-campaign
                cwe/police-notices
                cwe/create-free-trade-agreement
                cwe/create-capitalist-think-tank
                cwe/police-harass-recruitables
                cwe/fascist-demo]
        ;; DEBUG Replace probabilities with acctual values
        events-dbg (map #(assoc % :probability ((% :probability) g)) events)
        ]
    ;;[(cwui/event-menu events-dbg input)] ;; DEBUG
    (filter (partial rand-include-event? g) events)
    ))

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

      ;; Remove some recruitable if they aren't recruited
      (update-in [:recruitable] * 0.8)))

(defn update-opponent-power [g]
  (-> g
      (update-in [:fascists :power] cwo/adj-level + (* 0.02 (cws/fascist-activity g)))
      (update-in [:fascists :conflict] cwo/adj-level - 0.005)
      (update-in [:capitalists :power] + (* 0.02 (cws/capitalist-activity g)))))

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

(defn -main
  [& args]
  (play cwui/user-input))

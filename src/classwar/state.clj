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

(ns classwar.state
  (:require [lonocloud.synthread :as ->]))

(def initial-game-state
  "Create initial game state"
  {:day 0

   :activists                  5  ;; Number of
   :recruitable             0.00  ;; Possible recruits

   :organized-workforce     0.00  ;; %
   :money                   1000  ;; $$

   :fascists
   {:morale                 0.10
    :conflict               0.00
    :power                  0.01} ;; %

   :capitalists
   {:power                  0.50} ;; %

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
  (filter (comp #{:event} :type) (game :operations)))
(defn running-actions [game]
  (filter (comp #{:action} :type) (game :operations)))

(defn recruitable-activists [{r :recruitable}]
  (int (Math/floor r)))

(defn max-recruitment [{activists :activists :as g}]
  (let [space (- (activist-capacity g) activists)]
    (min space (recruitable-activists g))))

(def ACTIVIST_DAILY_DONATION 5)

(defn daily-donations [g]
  (let [free-activists (g :activists)
        bound-activists (keep :activists (g :insitutions))
        all-activists (reduce + free-activists bound-activists)]
    (* all-activists ACTIVIST_DAILY_DONATION)))

(def FASCIST_CYCLIC_PERIOD 100)
(defn- fa-cyclic-component [{day :day}]
  (let [PI (Math/PI)
        phase (- (/ PI 2.0))
        harmonic (Math/sin (+ (/ (* 2 PI day) FASCIST_CYCLIC_PERIOD) phase))]
    (/ (+ harmonic 1.0) 2.0)))

(defn- fa-climate-component [{pc :political-climate}]
  (let [CUTOFF 0.7
        pow-pc (Math/pow (/ (- CUTOFF pc) CUTOFF) 2.0)]
    (if (< pc CUTOFF) pow-pc 0.0)))

(defn fascist-activity [g]
  (let [cyclic (fa-cyclic-component g)
        climate (fa-climate-component g)
        conflict (get-in g [:fascists :conflict])
        morale (get-in g [:fascists :morale])]
    (+ (* 0.2 cyclic)
       (* 0.3 climate)
       (* 0.2 conflict)
       (* 0.3 morale))))

(defn- cap-climate-component [{pc :political-climate}]
  (let [CUTOFF 0.3
        pow-pc (Math/pow (/ (- CUTOFF pc) (- 1.0 CUTOFF)) 2.0)]
    (if (> pc CUTOFF) pow-pc 0.0))
  )
(defn capitalist-activity [g]
  (let [climate (cap-climate-component g)
        power (- 1.0 (get-in g [:capitalists :power]))]
    (+ (* 0.5 climate)
       (* 0.5 power))))

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

(ns classwar.events
  (:require [classwar.op :as cwo]
            [classwar.state :as cws]
            [lonocloud.synthread :as ->]))

(defmacro def-event [name desc params prob-fn op-fn]
  `(def ~name
     ~(merge {:id (keyword name)
              :type :event
              :desc desc
              :probability prob-fn
              :op `(cwo/op-helper ~op-fn)}
             params)))

(def-event nop-event
  "No event"
  {}
  (fn [g] 1.0)
  (fn [g a] g))

(def-event police-notices
  "Police takes notice of your movement"
  {}
  (fn [g]
    (if (g :police-noticed)
      0.0 ;; Police allready knows about movement
      (min 1.0 (/ (Math/sqrt (g :activists)) 100))))
  (fn [g a]
    (-> g
        (assoc :police-noticed true)
        (update-in [:police-repression] + 0.01)
        (update-in [:digest] conj "Police takes notice"))))

(def-event fascist-flyers
  "Fascist handout flyers"
  {}
  (fn [g]
    (min 0.5 (cws/fascist-activity g)))
  (fn [g a]
    (-> g
        (update-in [:fascists :power] cwo/adj-level + 0.01))))

(def-event fascist-posters
  "Fascist stick up posters"
  {:duration 3}
  (fn [g]
    (min 0.3 (cws/fascist-activity g)))
  (fn [g a]
    (-> g
        (->/when (cwo/first-day? a)
          (update-in [:digest] conj "Fascists stick up posters"))
        (update-in [:fascists :power] cwo/adj-level + 0.02))))

(def-event fascist-burn-comunity-center
  "Fascist burn down your comunity center"
  {}
  (fn [g]
    (let [activity (cws/fascist-activity g)]
      (cond
       (not (some #{:comunity-center} (map :id (-> g :institutions)))) 0.0
       (< activity 0.1) 0.0
       :else (* 0.1 activity))))
  (fn [g a]
    (let [center (first (filter #(= (% :id) :comunity-center) (g :institutions)))]
      (-> g
          ;; Remove the center
          (update-in [:institutions] disj center)

          ;; Let half of the activists return
          (update-in [:recruitable] + (* 0.5 (center :activists)))))))

(def-event police-evicts-occupied-building
  "Police evicts activists from occupied building"
  {}
  (fn [g]
    (let [repression (-> g :police-repression)]
      (cond
       (not (some #{:occupied-building} (map :id (-> g :institutions)))) 0.0
       :else (* 0.1 repression))))
  (fn [g a]
    (let [building (first (filter #(= (% :id) :occupied-building) (g :institutions)))]
      (-> g
          ;; Remove the building
          (update-in [:institutions] disj building)

          ;; Let most of the activists return
          (update-in [:recruitable] + (* 0.8 (building :activists)))))))


(def-event capitalist-ad-campaign
  "The capitalists run an ad campaign"
  {:duration 3}
  (fn [g]
    (min 0.1 (cws/capitalist-activity g)))
  (fn [g a]
    (-> g
        (->/when (cwo/first-day? a)
          (update-in [:capitalists :power] cwo/adj-level + 0.01)
          (update-in [:political-climate] cwo/adj-level - 0.01))

        ;; Every day
        (update-in [:political-climate] cwo/adj-level - 0.01))))

(defn free-trade-action [g a]
  (update-in g [:capitalists :power] cwo/adj-level + 0.001))

(def free-trade-agreement
  {:id :free-trade-agreement
   :type :institution
   :desc "Free trade agreement"
   :op free-trade-action})

(def-event create-free-trade-agreement
  "Free trade agreement"
  {}
  (fn [g]
    (* 0.1 (cws/capitalist-activity g)))
  (fn [g a]
    (-> g
        (update-in [:institutions] conj
                   free-trade-agreement))))


(defn capitalist-think-tank-action [g a]
  (update-in g [:capitalists :power] cwo/adj-level + 0.001))

(def capitalist-think-tank
  {:id :capitalist-think-tank
   :type :institution
   :desc "Capitalist think tank"
   :op capitalist-think-tank-action})

(def-event create-capitalist-think-tank
  "Capitalist think tank"
  {}
  (fn [g]
    (* 0.1 (cws/capitalist-activity g)))
  (fn [g a]
    (-> g
        (update-in [:institutions] conj
                   capitalist-think-tank))))

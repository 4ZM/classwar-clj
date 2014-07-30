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

(ns classwar.actions
  (:require [classwar.op :as cwo]
            [classwar.state :as cws]
            [lonocloud.synthread :as ->]))

(defmacro def-action [name desc params func]
  `(def ~name
     ~(merge {:id (keyword name)
              :type :action
              :desc desc
              :op `(cwo/op-helper ~func)}
             params)))

(def-action nop-action
  "Keep playing"
  {}
  (fn [g a] g))

(def-action surender
  "Give up"
  {}
  (fn [g a]
    (-> g
        (assoc-in [:status] :surender))))

(def-action antifa-demo
  "Organize antifa demonstration"
  {:effort 5}
  (fn [g a]
    (-> g
        (update-in [:recruitable] + 3.0)
        (update-in [:fascists :power] cwo/adj-level - 0.02)
        (update-in [:fascists :conflict] cwo/adj-level + 0.01)
        (update-in [:fascists :morale] cwo/adj-level - 0.01))))

(def-action anticap-demo
  "Orgnaize anti capitalist demonstration"
  {:effort 5}
  (fn [g a]
    (-> g
        (update-in [:recruitable] + 3.0)
        (update-in [:capitalists :power] cwo/adj-level - 0.02)
        (update-in [:political-climate] cwo/adj-level + 0.01))))

(def-action online-antifa-campaign
  "Start online antifa campaign"
  {:effort 2
   :duration 5
   :cost 500}
  (fn [g a]
    (-> g
        ;; First day
        (->/when (cwo/first-day? a)
          (update-in [:fascists :conflict] cwo/adj-level + 0.01)
          (update-in [:digest] conj
                     "You start an online antifa campaign and get some recruits"))

        ;; All days
        (update-in [:fascists :power] cwo/adj-level - 0.005)
        (update-in [:recruitable] + 1.0))))

(def-action party
  "Support party"
  {:effort 5
   :cost 1000}
  (fn [g a]
    (-> g
        (update-in [:recruitable] + 1.0)
        (update-in [:money] + 5000))))

(defn antifa-group-action [g institution]
  (update-in g [:fascists :morale] cwo/adj-level - 0.005))

(def antifa-group
  {:id :antifa-group
   :type :institution
   :desc "Antifa Group"
   :op antifa-group-action})

(def-action start-antifa-group
  "Start an antifa group"
  {:effort 5}
  (fn [g {activists :effort}]
    (-> g
        (update-in [:activists] - activists)
        (update-in [:institutions] conj
                   (merge antifa-group
                          {:activists activists})))))

(def-action anticap-flyers
  "Handout anti capitalist flyers"
  {:effort 1
   :cost 50}
  (fn [g a]
    (-> g
        (update-in [:capitalists :power] cwo/adj-level - 0.005)
        (update-in [:political-climate] cwo/adj-level + 0.001))))

(def-action antifa-flyers
  "Handout anti fascist flyers"
  {:effort 1
   :cost 50}
  (fn [g a]
    (-> g
        (update-in [:fascists :power] cwo/adj-level - 0.004))))

(def-action posters
  "Stick up posters"
  {:effort 4
   :cost 100
   :duration 7}
  (fn [g a]
    (-> g
        (->/when (cwo/first-day? a)
          (update-in [:police-repression] cwo/adj-level + 0.02))

        ;; Every day
        (update-in [:political-climate] cwo/adj-level + 0.001)
        (update-in [:recruitable] + 0.5))))

(def-action stickers
  "Stickers"
  {:effort 4
   :cost 200
   :duration 14}
  (fn [g a]
    (-> g
        (->/when (cwo/first-day? a)
          (update-in [:police-repression] cwo/adj-level + 0.02))

        ;; Every day
        (update-in [:political-climate] cwo/adj-level + 0.001)
        (update-in [:recruitable] + 0.3))))


(defn comunity-center-action [g institution]
  (update-in g [:political-climate] cwo/adj-level + 0.005))

(def comunity-center
  {:id :comunity-center
   :type :institution
   :desc "Comunity Center"
   :activist-capacity 30
   :op comunity-center-action})

(def-action start-comunity-center
  "Start a comunity center"
  {:effort 5
   :cost 1000}
  (fn [g {activists :effort}]
    (-> g
        (update-in [:activists] - activists)
        (update-in [:institutions] conj
                   (merge {:activists activists}
                          comunity-center)))))


(def-action reclaim-party
  "Reclaim the Streets party"
  {:effort 20}
  (fn [g a]
    (-> g
        (update-in [:recruitable] + 5.0)
        (update-in [:political-climate] cwo/adj-level + 0.005)
        (update-in [:police-repression] cwo/adj-level + 0.10))))

(defn union-action [g institution]
  (-> g
      (update-in [:political-climate] cwo/adj-level + 0.001)
      (update-in [:organized-workforce] cwo/adj-level + 0.005)))

(def union
  {:id :union
   :type :institution
   :desc "Union"
   :op union-action})


(def-action start-union
  "Start a Union"
  {:effort 10}
  (fn [g {activists :effort}]
    (-> g
        (update-in [:activists] - activists)
        (update-in [:institutions] conj
                   (merge {:activists activists} union)))))


(def-action revolution
  "Start Revolution"
  {:effort 10}
  (fn [g a]
    (-> g
        (assoc-in [:status] :revolution))))

(defn occupied-building-action [g a]
  (update-in g [:political-climate] cwo/adj-level + 0.001))

(def occupied-building
  {:id :occupied-building
   :type :institution
   :desc "Occupied Building"
   :activist-capacity 20
   :op occupied-building-action})

(def-action occupy-building
  "Occupy abandoned building"
  {:effort 5}
  (fn [g {activists :effort}]
    (-> g
        (update-in [:activists] - activists)
        (update-in [:institutions] conj
                   (merge {:activists activists}
                          occupied-building)))))

(def-action tear-down-fascist-propaganda
  "Tear down fascist propaganda"
  {:effort 5}
  (fn [g a]
    (let [posters (cws/running-op g :fascist-posters)]
      (-> g
          (update-in [:fascists :conflict] cwo/adj-level + 0.02)
          (update-in [:fascists :morale] cwo/adj-level - 0.01)
          (cwo/remove-op posters)))))

(def-action adbusting
  "Adbusting capitalist propaganda"
  {:effort 3}
  (fn [g a]
    (let [ads (cws/running-op g :capitalist-ad-campaign)]
      (-> g
          (update-in [:police-repression] cwo/adj-level + 0.02)
          (update-in [:political-climate] cwo/adj-level + 0.003)
          (cwo/remove-op ads)))))

(def-action counter-fascist-demo
  "Counter protest fascist demo"
  {:effort 5}
  (fn [g a]
    (let [fa-demo (cws/running-op g :fascist-demo)]
      (-> g
          (update-in [:digest] conj
                     "Your counter demo send the fascists running away")
          (update-in [:police-repression] cwo/adj-level + 0.01)
          (update-in [:fascists :conflict] cwo/adj-level + 0.10)
          (update-in [:fascists :morale] cwo/adj-level - 0.05)
          (update-in [:fascists :power] cwo/adj-level - 0.01)
          (cwo/remove-op fa-demo)))))



(def strike)

(def occupy-university)
(def start-anticap-group)
(def start-adbusting-group)
(def start-paper)
(def start-book-cafe)
(def build-printshop)

(def start-hackerspace)
(def start-activist-food-truck)
(def create-web-site)

(def general-strike)

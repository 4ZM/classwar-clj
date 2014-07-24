(ns classwar.events
  (:require [classwar.op :as cwo]
            [lonocloud.synthread :as ->]))

(def nop-event
  {:id :nop
   :type :event
   :desc "No event"
   :probability (fn [g] 1.0)
   :action (cwo/op-helper (fn [g a] g))})


(def police-notices
  {:id :police-notices
   :type :event
   :desc "Police takes notice of your movement"
   :probability
   (fn [g]
     (if (g :police-noticed)
       0.0 ;; Police allready knows about movement
       (min 1.0 (/ (Math/sqrt (g :activists)) 100))))
   :action
   (cwo/op-helper
    (fn [g a]
      (-> g
          (assoc :police-noticed true)
          (update-in [:police-repression] + 0.01)
          (update-in [:digest] conj "Police takes notice"))))})

(def fascist-flyers
  {:id :fascist-flyers
   :type :event
   :desc "Fascist handout flyers"
   :probability
   (fn [g]
     (let [pow (-> g :fascists :power)
           act (-> g :fascists :activity)]
       (max 0.01 (min pow act))))
   :action
   (cwo/op-helper
    (fn [g a]
      (-> g
          (update-in [:fascists :power] adj-level + 0.01))))})

(def fascist-burn-comunity-center
  {:id :fascist-burn-comunity-center
   :type :event
   :desc "Fascist burn down your comunity center"
   :probability
   (fn [g]
     (let [pow (-> g :fascists :power)
           activity (-> g :fascists :activity)]
       (cond
        (not (some #{:comunity-center} (map :id (-> g :institutions)))) 0.0
        (< activity 0.1) 0.0
        :else (* 0.1 activity))))
   :action
   (cwo/op-helper
    (fn [g a]
      (let [center (first (filter #(= (% :id) :comunity-center) (g :institutions)))]
        (-> g
            ;; Remove the center
            (update-in [:institutions] disj center)

            ;; Let half of the activists return
            (update-in [:recruitable] + (quot (center :activists) 2))))))})

(def police-evicts-occupied-building
  {:id :police-evicts-occupied-building
   :type :event
   :desc "Police evicts activists from occupied building"
   :probability
   (fn [g]
     (let [repression (-> g :police-repression)]
       (cond
        (not (some #{:occupied-building} (map :id (-> g :institutions)))) 0.0
        :else (* 0.1 repression))))
   :action
   (cwo/op-helper
    (fn [g a]
      (let [building (first (filter #(= (% :id) :occupied-building) (g :institutions)))]
        (-> g
            ;; Remove the building
            (update-in [:institutions] disj building)

            ;; Let half of the activists return
            (update-in [:recruitable] + (quot (building :activists) 2))))))})


(def capitalist-ad-campaign
  {:id :capitalist-ad-campaig
   :type :event
   :desc "The capitalists run an ad campaign"
   :duration 3
   :probability
   (fn [g]
     (let [pow (-> g :capitalists :power)
           act (-> g :capitalists :activity)]
       (max 0.01 (min pow act))))
   :action
   (cwo/op-helper
    (fn [g a]
      (-> g
          (->/when (first-day? a)
            (update-in [:capitalists :power] adj-level + 0.01)
            (update-in [:political-climate] adj-level - 0.01))

          ;; Every day
          (update-in [:political-climate] adj-level - 0.01))))})


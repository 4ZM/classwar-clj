(ns classwar.events
  (:require [classwar.op :as cwo]
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
    (let [pow (-> g :fascists :power)
          act (-> g :fascists :activity)]
      (max 0.01 (min pow act))))
  (fn [g a]
    (-> g
        (update-in [:fascists :power] cwo/adj-level + 0.01))))

(def-event fascist-posters
  "Fascist stick up posters"
  {:duration 3}
  (fn [{{power :power activity :activity} :fascists}]
    (max 0.01 (min power activity)))
  (fn [g a]
    (-> g
        (->/when (cwo/first-day? a)
          (update-in [:digest] conj "Fascists stick up posters"))
        (update-in [:fascists :power] cwo/adj-level + 0.02))))

(def-event fascist-burn-comunity-center
  "Fascist burn down your comunity center"
  {}
  (fn [g]
    (let [pow (-> g :fascists :power)
          activity (-> g :fascists :activity)]
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
          (update-in [:recruitable] + (quot (center :activists) 2))))))

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

          ;; Let half of the activists return
          (update-in [:recruitable] + (quot (building :activists) 2))))))


(def-event capitalist-ad-campaign
  "The capitalists run an ad campaign"
  {:duration 3}
  (fn [g]
    (let [pow (-> g :capitalists :power)
          act (-> g :capitalists :activity)]
      (max 0.01 (min pow act))))
  (fn [g a]
    (-> g
        (->/when (cwo/first-day? a)
          (update-in [:capitalists :power] cwo/adj-level + 0.01)
          (update-in [:political-climate] cwo/adj-level - 0.01))

        ;; Every day
        (update-in [:political-climate] cwo/adj-level - 0.01))))


(ns classwar.events
  (:require [lonocloud.synthread :as ->]))

(defn- remove-event [g a]
  (update-in g [:events] disj a))

(defn cost [a] (get a :cost 0))
(defn effort [a] (get a :effort 0))
(defn duration [a] (get a :duration 1))
(defn- running [a] (get a :running 1))

(defn- first-day? [a] (= (running a) 1))
(defn- last-day? [a] (= (running a) (duration a)))

(defn- update-event [g old-event new-event]
  (-> g
      (update-in [:events] disj old-event)
      (update-in [:events] conj new-event)))

(defn- adj-level [level op val]
  (max (min 1.0 (op level val)) 0.0))

(defn- event-helper [func]
  (fn [g a]
    (-> g

        ;; Spend moneyz on the first day
        (cond-> (first-day? a)
                (update-in [:money] - (cost a)))

        ;; Run event
        (func a)

        (->/if (last-day? a)
          ;; Remove event on last day
          (remove-event a)

          ;; Count up running days
          (update-event a (assoc a :running (inc (running a))))))))

(def nop
  {:id :nop
   :desc "No event"
   :probability (fn [g] 1.0)
   :action (event-helper (fn [g a] g))})


(def police-notices
  {:id :police-notices
   :desc "Police takes notice of your movement"
   :probability
   (fn [g]
     (if (g :police-noticed)
       0.0 ;; Police allready knows about movement
       (min 1.0 (/ (Math/sqrt (g :activists)) 100))))
   :action(event-helper
           (fn [g a]
             (println "EVENT! Police takes notice!")
             (-> g
                 (assoc :police-noticed true)
                 (update-in [:police-repression] + 0.01))))})


(def fascist-flyers
  {:id :fascist-flyers
   :desc "Fascist handout flyers"
   :probability
   (fn [g]
     (let [pow (-> g :fascists :power)
           act (-> g :fascists :activity)]
       (max 0.01 (min pow act))))
   :action
   (event-helper
    (fn [g a]
      (-> g
          (update-in [:fascists :power] adj-level + 0.01))))})

(def fascist-burn-comunity-center
  {:id :fascist-burn-comunity-center
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
   (event-helper
    (fn [g a]
      (let [center (first (filter #(= (% :id) :comunity-center) (g :institutions)))]
        (-> g
            ;; Remove the center
            (update-in [:institutions] disj center)

            ;; Let half of the activists return
            (update-in [:recruitable] + (quot (center :activists) 2))))))})

(def police-evicts-occupied-building
  {:id :police-evicts-occupied-building
   :desc "Police evicts activists from occupied building"
   :probability
   (fn [g]
     (let [repression (-> g :police-repression)]
       (cond
        (not (some #{:occupied-building} (map :id (-> g :institutions)))) 0.0
        :else (* 0.1 repression))))
   :action
   (event-helper
    (fn [g a]
      (let [building (first (filter #(= (% :id) :occupied-building) (g :institutions)))]
        (-> g
            ;; Remove the building
            (update-in [:institutions] disj building)

            ;; Let half of the activists return
            (update-in [:recruitable] + (quot (building :activists) 2))))))})


(def capitalist-ad-campaign
  {:id :capitalist-ad-campaig
   :desc "The capitalists run an ad campaign"
   :duration 3
   :probability
   (fn [g]
     (let [pow (-> g :capitalists :power)
           act (-> g :capitalists :activity)]
       (max 0.01 (min pow act))))
   :action
   (event-helper
    (fn [g a]
      (-> g
          (->/when (first-day? a)
            (update-in [:capitalists :power] adj-level + 0.01)
            (update-in [:political-climate] adj-level - 0.01))

          ;; Every day
          (update-in [:political-climate] adj-level - 0.01))))})


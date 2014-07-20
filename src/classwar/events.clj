(ns classwar.events
  (:require [lonocloud.synthread :as ->]))

(defn- remove-event [g a]
  (update-in g [:events] disj a))

(defn- extended-event? [a] (contains? a :duration))

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
   :desc "Keep playing"
   :effort 0
   :action (event-helper (fn [g a] g))})


(def police-notices
  {:id :police-notices
   :desc "Police takes notice of your movement"
   :cond (fn [g] (and (> (g :activists) 9) (not (g :police-noticed))))
   :action(event-helper
           (fn [g a]
             (println "EVENT! Police takes notice!")
             (-> g
                 (assoc :police-noticed true)
                 (update-in [:police-repression] + 0.01))))})


(def fascist-flyers
  {:id fascist-flyers
   :desc "Fascist handout flyers"
   :action
   (event-helper
    (fn [g a]
      (-> g
          (update-in [:fascists :power] adj-level + 0.01))))})

(def fascist-burn-comunity-center
  {:id fascist-burn-comunity-center
   :desc "Fascist burn down your comunity center"
   :action
   (event-helper
    (fn [g a]
      (let [center (first (filter #(= (% :id) :comunity-center) (g :institutions)))]
        (-> g
            ;; Remove the center
            (update-in [:institutions] disj center)

            ;; Let half of the activists return
            (update-in [:prospects] + (quot (center :activists) 2))))))})

(def capitalist-ad-campaign
  {:id :capitalist-ad-campaig
   :desc "The capitalists run an ad campaign"
   :duration 3
   :action
   (event-helper
    (fn [g a]
      (-> g
          (->/when (first-day? a)
            (update-in [:capitalists :power] adj-level + 0.01)
            (update-in [:political-climate] adj-level - 0.01))

          ;; Every day
          (update-in [:political-climate] adj-level - 0.01))))})


(defn current-events [g a]
  (filter #((% :cond) g) all-events))

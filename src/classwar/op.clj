(ns classwar.op
  (:require [lonocloud.synthread :as ->]))


(defn cost [a] (get a :cost 0))
(defn effort [a] (get a :effort 0))
(defn duration [a] (get a :duration 1))
(defn running [a] (get a :running 1))


(defn adj-level [level op val]
  (max (min 1.0 (op level val)) 0.0))


(defn first-day? [a] (= (running a) 1))
(defn last-day? [a] (= (running a) (duration a)))

(defn remove-op [g a] (update-in g [:operations] disj a))
(defn- update-op [g old-event new-event]
  (-> g
      (update-in [:operations] disj old-event)
      (update-in [:operations] conj new-event)))


(defn op-helper [func]
  (fn [g a]
    (-> g

        ;; Remove moneyz on the first day
        (cond-> (first-day? a) (update-in [:money] - (cost a)))

        ;; Run operation
        (func a)

        ;; Remove operation on last day, or count up running days
        (->/if (last-day? a)
          (remove-op a)
          (update-op a (assoc a :running (inc (running a))))))))

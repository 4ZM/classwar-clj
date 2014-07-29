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

(ns classwar.op
  (:require [lonocloud.synthread :as ->]))


(defn cost [a] (get a :cost 0))
(defn effort [a] (get a :effort 0))
(defn duration [a] (get a :duration 1))
(defn running [a] (get a :running 1))


(defn adj-level [level op val]
  (max (min 1.0 (op level val)) 0.0))


(defn first-day? [a] (if (nil? a) false (= (running a) 1)))
(defn last-day? [a] (if (nil? a) false (= (running a) (duration a))))

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

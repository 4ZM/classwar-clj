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

(ns classwar.ui
  (:require [classwar.op :as cwo]
            [classwar.state :as cws]
            [classwar.actions :as cwa]
            [classwar.events :as cwe]
            [clojure.string :as str]
            [lonocloud.synthread :as ->]))

(defn format-level [v old-v]
  (let [marks-len 20
        n-marks (Math/round (* marks-len v))
        meter (str/join "" (repeat n-marks "#"))
        spc (str/join "" (repeat (- marks-len n-marks) "-"))]
    (format "[%1.3f] [%s%s] [%+1.3f]" v meter spc (- v old-v))))

(defn show-game-overview [g last-g]
  (println (format "\nDay %d  -  Game Overview" (g :day)))
  (println (format "  Activists: %d [%d]  Money: $%d [%d]"
                   (g :activists)
                   (- (g :activists) (last-g :activists))
                   (g :money)
                   (- (g :money) (last-g :money))))
  (println (format "  Recruitable: %2.2f [%2.2f]  Capacity: %d [%d]"
                   (g :recruitable)
                   (- (g :recruitable) (last-g :recruitable))
                   (cws/activist-capacity g) (cws/activist-capacity last-g)))
  (println (format "  Org. Workforce       %s"
                   (format-level (g :organized-workforce)
                                 (last-g :organized-workforce))))
  (println (format "  Fascist Power        %s"
                   (format-level (-> g :fascists :power)
                                 (-> last-g :fascists :power))))
  (println (format "  Fascist Activity     %s"
                   (format-level (cws/fascist-activity g)
                                 (cws/fascist-activity last-g))))
  (println (format "  Fascist Conflict     %s"
                   (format-level (-> g :fascists :conflict)
                                 (-> last-g :fascists :conflict))))
  (println (format "  Fascist Morale       %s"
                   (format-level (-> g :fascists :morale)
                                 (-> last-g :fascists :morale))))
  (println (format "  Capitalist Power     %s"
                   (format-level (-> g :capitalists :power)
                                 (-> last-g :capitalists :power))))
  (println (format "  Capitalist Activity  %s"
                   (format-level (cws/capitalist-activity g)
                                 (cws/capitalist-activity last-g))))
  (println (format "  Police Repression    %s"
                   (format-level (g :police-repression)
                                 (last-g :police-repression))))
  (println (format "  Political Climate    %s"
                   (format-level (g :political-climate)
                                 (last-g :political-climate))))

  (println "  Institutions:" (str/join ", " (map :desc (g :institutions))))
  (println "  Actions:" (str/join ", " (map :id (cws/running-actions g))))
  (println "  Events:" (str/join ", " (map :id (cws/running-events g))))
  (println "  Messages:")
  (println " " (str/join "\n  " (g :digest))))

(defn- format-menu-option [i op]
  (let [desc (op :desc)
        effort (cwo/effort op)
        prob (op :probability)]
    (if (= (op :type) :event)
      (format "  %d. %s [%2.2f]" i desc (* 100 prob))
      (format "  %d. %s [%d A]" i desc effort))))

(defn user-input []
  (let [str-opt (read-line)]
    (Integer/parseInt str-opt)))

(defn vec-input [v]
  (let [va (atom v)]
    (fn []
      (let [x (first @va)]
        (swap! va rest)
        x))))

(defn action-menu [opts input]
  (println "Action Menu")
  (println (str/join "\n" (map-indexed format-menu-option opts)))
  (print "  Select: ")
  (let [sel (input)]
    (println sel)
    (nth opts sel)))

(defn event-menu [opts input]
  (println "DEBUG: Events Menu")
  (println (str/join "\n" (map-indexed format-menu-option opts)))
  (print "  Select: ")
  (let [sel (input)]
    (println sel)
    (nth opts sel)))

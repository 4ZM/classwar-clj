(ns classwar.ui
  (:require [classwar.op :as cwo]
            [classwar.state :as cws]
            [classwar.actions :as cwa]
            [classwar.events :as cwe]
            [clojure.string :as str]
            [lonocloud.synthread :as ->]))



(defn show-game-overview [g last-g]
  (println (format "Game overview, day %d" (g :day)))
  (println (format "  Activists: %d [%d]  Recruitable: %2.2f [%2.2f]"
                   (g :activists)
                   (- (g :activists) (last-g :activists))
                   (g :recruitable)
                   (- (g :recruitable) (last-g :recruitable))))
  (println (format "  Workforce: %2.2f [%2.2f]  Money: %d [%d]"
                   (g :organized-workforce)
                   (- (g :organized-workforce) (last-g :organized-workforce))
                   (g :money)
                   (- (g :money) (last-g :money))))
  (println (format "  Fascists>      Power:    %2.2f [%2.2f]  Activity: %2.2f [%2.2f]"
                   (-> g :fascists :power)
                   (- (-> g :fascists :power) (-> last-g :fascists  :power))
                   (cws/fascist-activity g)
                   (- (cws/fascist-activity g) (cws/fascist-activity last-g))))
  (println (format "  DBG Fascists>  Conflict: %2.2f [%2.2f]  Morale:   %2.2f [%2.2f]"
                   (-> g :fascists :conflict)
                   (- (-> g :fascists :conflict) (-> last-g :fascists  :conflict))
                   (-> g :fascists :morale)
                   (- (-> g :fascists :morale) (-> last-g :fascists  :morale)))) 
  (println (format "  Capitalists>   Power: %2.2f [%2.2f]  Activity: %2.2f [%2.2f]"
                   (-> g :capitalists :power)
                   (- (-> g :capitalists :power) (-> last-g :capitalists :power))
                   (-> g :capitalists :activity)
                   (- (-> g :capitalists :activity) (-> last-g :capitalists :activity))))
  (println (format "  Police Repression: %2.2f [%2.2f]  Political Climate: %2.2f [%2.2f]"
                   (g :police-repression)
                   (- (g :police-repression) (last-g :police-repression))
                   (g :political-climate)
                   (- (g :political-climate) (last-g :political-climate))))

  (println (format "  Activist Capacity: %d [%d]"
                   (cws/activist-capacity g) (cws/activist-capacity last-g)))
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

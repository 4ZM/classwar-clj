(ns classwar.ui
  (:require [classwar.actions :as cwa]
            [classwar.events :as cwe]
            [clojure.string :as str]
            [lonocloud.synthread :as ->]))



(defn show-game-overview [g]
  (println (format "Game overview, day %d" (g :day)))
  (println (format "  Activists: %d  Workforce: %2.2f  Revolution: %2.2f"
                   (g :activists)
                   (g :organized-workforce)
                   (g :revolutionary-potential)))
  (println (format "  Fascists>    Power: %2.2f  Activity: %2.2f"
                   (-> g :fascists :power)
                   (-> g :fascists :activity)))
  (println (format "  Capitalists> Power: %2.2f  Activity: %2.2f"
                   (-> g :capitalists :power)
                   (-> g :capitalists :activity)))
  (println (format "  Police Repression: %2.2f  Political Climate: %2.2f"
                   (g :police-repression)
                   (g :political-climate))))

(defn- format-menu-option [i opt]
  (format "  %d. %s [%d A]" i (opt :desc) (opt :effort)))

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

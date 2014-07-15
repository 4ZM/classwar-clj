(ns classwar.events)

(def police-notices
  {:id :police-notices
   :desc "Police takes notice of your movement"
   :cond (fn [g] (and (> (g :activists) 9) (not (g :police-noticed))))
   :action (fn [g]
             (println "EVENT! Police takes notice!")
             (-> g
                 (assoc-in [:police-noticed] true)
                 (update-in [:police-repression] + 0.01)))})

(def all-events
  [police-notices])

(defn current-events [g a]
  (filter #((% :cond) g) all-events))

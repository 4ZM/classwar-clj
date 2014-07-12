(ns classwar.events)

(def police-notices
  {:id :police-notices
   :desc "Police takes notice of your movement"
   :cond (fn [g] (and (> (g :activists) 10) (not (g :police-noticed))))
   :action (fn [g] (-> g
                      (assoc-in [:police-noticed] true)
                      (update-in [:police-repression] + 0.01)))})


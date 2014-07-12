(ns classwar.actions)

(def nop
  {:id :nop
   :desc "Keep playing"
   :effort 0
   :action (fn [g _ _ _] g)})

(def surender
  {:id :surender
   :desc "Give up"
   :duration 0
   :effort 0
   :action (fn [g a _ _] (assoc-in g [:status] :game-over))})


(defn demo-action [g a _ _]
  ;; Return result structure with rationale
  (cond
   (= (a :type) :antifa)
   (-> g
       (update-in [:facist-activity] - 0.01) ;; fight facists
       (update-in [:activists] + 2)) ;; recruit activists
   (= (a :type) :anticap) (update-in g [:capitalist-activity] - 0.01)))

(def demo
  {:id :demo
   :desc "Organize demonstration"
   :effort 10
   :type :antifa
   :action demo-action})


(defn online-campaign-action [g a _ _]
  (-> g
      (update-in [:activists] + 1)
      (update-in [:facist-activity] - 0.01)
      (update-in [:political-climate] - 0.01)))

(def online-campaign
  {:id :online-campaign
   :desc "Start online campaign"
   :effort 2
   :action online-campaign-action})


(defn party-action [g a _ _]
  (-> g
      (update-in [:activists] + 1)
      (update-in [:money] + 5000)))

(def party
  {:id :party
   :desc "Support party"
   :effort 5
   :action party-action})

(defn antifa-group-action [g institution _ _]
  (update-in g [:facist-activity] - 0.01))

(defn start-antifa-group-action [g a _ _]
  (let [req-activists (a :effort)]
    (-> g
        (update-in [:activists] - req-activists)
        (update-in [:institutions] conj
                   {:type :antifa-group
                    :activists req-activists
                    :action antifa-group-action}))))

(def start-antifa-group
  {:id :antifa-group
   :desc "Start an antifa group"
   :effort 5
   :action start-antifa-group-action})

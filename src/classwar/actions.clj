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
   :action (fn [g _ _ _] (assoc-in g [:status] :game-over))})

(defn adj-level [level op val]
  (max (min 1.0 (op level val)) 0.0))

(def demo-template
  {:id :demo
   :desc "Organize demonstration"
   :type :tmp
   :effort :tmp
   :action
   (fn [g a _ _]
     (cond
      (= (a :type) :antifa)
      (-> g
          (update-in [:facists :activity] adj-level - 0.01) ;; fight facists
          (update-in [:activists] + 2)) ;; recruit activists
      (= (a :type) :anticap) (update-in g [:capitalists :activity] - 0.01)))})

(defn create-demo [type activists]
  (-> demo
      (assoc-in [:type] type)
      (assoc-in [:effort] activists)
      (assoc-in [:desc] (cond
                         (= type :antifa)
                         "Organize Antifa demonstration"
                         (= type :anticap)
                         "Orgnaize anti-capitalist demonstration"))))

(def online-campaign
  {:id :online-campaign
   :desc "Start online campaign"
   :effort 2
   :action
   (fn [g _ _ _]
     (-> g
         (update-in [:activists] + 1)
         (update-in [:fascists :activity] adj-level - 0.01)
         (update-in [:political-climate] - 0.01)))})

(def party
  {:id :party
   :desc "Support party"
   :effort 5
   :action
   (fn [g _ _ _]
     (-> g
         (update-in [:activists] + 1)
         (update-in [:money] + 5000)))})

(defn antifa-group-action [g institution _ _]
  (update-in g [:fascists :activity] adj-level - 0.01))

(def start-antifa-group
  {:id :antifa-group
   :desc "Start an antifa group"
   :effort 5
   :action
   (fn [g a _ _]
     (let [req-activists (a :effort)]
       (-> g
           (update-in [:activists] - req-activists)
           (update-in [:institutions] conj
                      {:type :antifa-group
                       :activists req-activists
                       :action antifa-group-action}))))})

(def handout-flyers
  {:id :handout-flyers
   :desc "Handout flyers"
   :effort 1
   :action
   (fn [g _ _ _]
     (update-in g [:revolutionary-potential] adj-level + 0.01))})

(def posters
  {:id :posters
   :desc "Stick up posters"
   :effort 2
   :action
   (fn [g _ _ _]
     (-> g
         (update-in [:police-repression] adj-level + 0.01)
         (update-in [:revolutionary-potential] adj-level + 0.01)))})

(def stickers
  {:id :posters
   :desc "Stickers"
   :effort 2
   :action
   (fn [g _ _ _]
     (-> g
         (update-in [:police-repression] adj-level + 0.01)
         (update-in [:revolutionary-potential] adj-level + 0.01)))})

(def reclaim-party)
(def occupy-university)


(def start-union)
(def start-anticap-group)
(def start-adbusting-group)
(def start-paper)
(def start-book-cafe)
(def start-comunity-center)

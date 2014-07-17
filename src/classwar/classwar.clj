(ns classwar
  (:require [classwar.actions :as cwa]
            [classwar.events :as cwe]
            [classwar.ui :as cwui]
            [lonocloud.synthread :as ->]))

(defn activist-capacity [g]
  "Max number of activists that can be organized"
  (cond
   (some #{:comunity-center} (map :id (g :institutions))) 30
   :default 10
   ))

(defn setup-game []
  "Create initial game state"
  {:day 0

   :activists                  5  ;; Number of
   :prospects                  0  ;; Possible recruits

   :revolutionary-potential 0.00  ;; %
   :organized-workforce     0.00  ;; %
   :money                   1000  ;; $$

   :fascists {:activity     0.05  ;; %
             :power         0.01} ;; %

   :capitalists {:activity  0.10  ;; %
                 :power     0.50} ;; %

   :police-repression       0.00  ;; %

   :political-climate       0.00  ;; -1 to 1 (left / right)

   :institutions              []  ;; Support groups and structures

   :police-noticed         false  ;; Police knows about the movement

   :actions                   []  ;; All actions (indexed by day)
   :events                    []  ;; All events (indexed by day)

   :status :running})

(defn has-institution? [id {institutions :institutions}]
  (some #{id} (map :id institutions)))

(defn nil-or-lt? [val other]
  "true if val is nil or < other"
  (or (nil? val) (< val other)))

(defn revolution-available? [g]
  (and (has-institution? :union g)
       (has-institution? :comunity-center g)
       (has-institution? :antifa-group g)))

(defn available-options [g available-activists available-money]
  (let [actions [cwa/nop
                 cwa/surender
                 (cwa/create-demo :antifa 5)
                 (cwa/create-demo :anticap 5)
                 cwa/handout-flyers
                 cwa/posters
                 cwa/stickers
                 cwa/online-campaign
                 cwa/party
                 cwa/reclaim-party]
        activist-filter (partial filter #(nil-or-lt? (% :effort) available-activists))
        cost-filter (partial filter #(nil-or-lt? (% :cost) available-money))]
    (-> actions
        (cond-> (not (has-institution? :union g))
                (conj cwa/start-union))
        (cond-> (not (has-institution? :comunity-center g))
                (conj cwa/start-comunity-center))
        (cond-> (not (has-institution? :antifa-group g))
                (conj cwa/start-antifa-group))
        (cond-> (revolution-available? g)
                (conj cwa/revolution))
        activist-filter
        cost-filter)))

(defn get-actions [g input]
  (let [available-activists (g :activists)
        opts (available-options g available-activists (g :money))]
    [(cwui/action-menu opts input)]))


(defn todays-actions [{day :day actions :actions}]
  (if (< day (count actions))
    (actions day)
    []))

(defn todays-events [{day :day events :events}]
  (if (< day (count events))
    (events day)
    []))


(defn execute-actions [game]
  (let [todays (todays-actions game)
        todays-expenses (reduce + (keep :cost todays))
        action-fns (map (fn [a] (fn [g] ((a :action) g a))) todays)]

    (-> game
        (update-in [:money] - todays-expenses)
        ((apply comp action-fns)))))

(defn institution-updates [game]
  (let [institution-fns (map (fn [i] (fn [g] ((i :action) g i)))
                             (game :institutions))]
    ((apply comp institution-fns) game)))

(defn execute-events [game]
  (let [event-fns (map :action (todays-events game))]
    ((apply comp event-fns) game)))



(defn max-recruitment [{activists :activists prospects :prospects :as g}]
  (let [space (- (activist-capacity g) activists)]
    (min space prospects)))

(defn recruit-activists [g]
  (-> g
      (->/as (-> max-recruitment new-activists)
             (update-in [:activists] + new-activists)) ; update will be in clj 1.7
      (assoc :prospects 0)))

(defn update-opponent-power [g]
  (-> g
      (update-in [:fascists :power] + (* 0.1 (-> g :fascists :activity)))
      (update-in [:capitalists :power] + (* 0.1 (-> g :capitalists :activity)))))

(defn update-game-status [g]
  (-> g
      (cond->
       (some #{:revolution} (map :id (todays-actions g)))
       (assoc :status :revolution))
      (cond->
       (>= (-> g :fascists :power) 1.0)
       (assoc :status :fascists-win))
      (cond->
       (>= (-> g :capitalists :power) 1.0)
       (assoc :status :capitalists-win))))

(defn last-n-days-actions [n {actions :actions}]
  (flatten (take n (reverse actions))))

(defn some-action-last-n-days [n tag {actions :actions :as game}]
  (some #{tag} (map :id (last-n-days-actions n game))))

(defn collect-money [g]
  (let [free-activists (g :activists)
        bound-activists (keep :activists (g :insitutions))
        all-activists (reduce + free-activists bound-activists)]
    (* all-activists 5)))

(defn tic [game actions events]
  (-> game
      (update-in [:actions] conj actions)
      (update-in [:events] conj events)
      execute-actions
      institution-updates
      execute-events

      ;; Posters have some effect for 3 days
      (->/as game
             (cond-> (some-action-last-n-days 3 :posters game)
                     (update-in [:revolutionary-potential] cwa/adj-level + 0.01)))
      ;; Stickers have some effect for 5 days
      (->/as game
             (cond-> (some-action-last-n-days 5 :stickers game)
                     (update-in [:revolutionary-potential] cwa/adj-level + 0.01)))

      ;; Collect money from members
      (->/as game
             (update-in [:money] + (collect-money game)))

      recruit-activists
      update-opponent-power
      update-game-status
      (update-in [:day] inc)))

(defn game-over [g]
  (println "GAME OVER")
  (case (g :status)
    :surender (println " > You give up")
    :revolution (println " > Revolution! You win!")
    :fascists-win (println " > Fascists Win - You lose")
    :capitalists-win (println " > Capitalists Win - You lose")))

(defn play [input]
  (loop [g  (setup-game)]
    (cwui/show-game-overview g)
    (let [actions (get-actions g input)
          events (cwe/current-events g actions)
          g (tic g actions events)]
      (if (= (g :status) :running)
        (recur g)
        (game-over g)))))

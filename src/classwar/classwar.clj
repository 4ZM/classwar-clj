(ns classwar
  (:require [classwar.actions :as cwa]
            [classwar.events :as cwe]
            [classwar.ui :as cwui]
            [lonocloud.synthread :as ->]))

(defn activist-capacity [g]
  "Max number of activists that can be organized"
  ;; Inc with better infrastructure
  10)

(defn setup-game []
  "Create initial game state"
  {:day 1

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

   :status :running})

(defn available-options [g available-activists]
  (let [actions [cwa/nop
                 cwa/surender
                 (cwa/create-demo :antifa 5)
                 (cwa/create-demo :anticap 5)
                 cwa/handout-flyers
                 cwa/posters
                 cwa/stickers
                 cwa/online-campaign
                 cwa/party
                 cwa/start-antifa-group]]
    (filter #(< (% :effort) available-activists) actions)))

(defn get-actions [g input]
  (let [available-activists (g :activists)
        opts (available-options g available-activists)]
    [(cwui/action-menu opts input)]))

(defn tic [game actions events]

  (let [g (atom game)]

    ;; Do all the actions
    ;; (doall
    ;;  (for [a actions]
    ;;    (reset! g ((a :action) @g a actions events))))

    (let [action-fns (map (fn [a] (fn [g] ((a :action) g a actions events))) actions)
          action-chain-fn (apply comp action-fns)]
      (reset! g (action-chain-fn @g)))

    ;; Let institutions act
    (doall
     (for [i (@g :institutions)]
       (reset! g ((i :action) @g i actions events))))

    ;; Let events play out
    (doall
     (for [e events]
       (do (println "EVENT! " (e :desc))
           (reset! g ((e :action) @g)))))

    ;; Do game logic

    ;; Get new activists
    (let [space (- (activist-capacity @g) (@g :activists))
          new-activists (min space (@g :prospects))]
      (reset! g (update-in @g [:activists] + new-activists)))
    (reset! g (assoc-in @g [:prospects] 0))

    ;; Update opponents power
    (reset! g (update-in @g [:fascists :power] + (-> game :fascists :activity)))
    (reset! g (update-in @g [:capitalists :power] + (-> game :capitalists :activity)))

    ;; Check termination
    (if (>= (-> @g :fascists :power) 1.0) (reset! g (assoc-in @g [:status] :fascists-win)))

    ;; Advance to next day
    (reset! g (update-in @g [:day] inc))

    ;; Return new game state
    @g))

(defn game-over [g]
  (println "GAME OVER")
  (case (g :status)
    :surender (println " > You give up")
    :fascists-win (println " > Fascists Win - You lose")))

(defn play [input]
  (loop [g  (setup-game)]
    (cwui/show-game-overview g)
    (let [actions (get-actions g input)
          events (cwe/current-events g actions)
          g (tic g actions events)]
      (if (= (g :status) :running)
        (recur g)
        (game-over g)))))

(ns classwar.actions
  (:require [lonocloud.synthread :as ->]))

(defn- remove-action [g a]
  (update-in g [:actions] disj a))

(defn- extended-action? [a] (contains? a :duration))

(defn cost [a] (get a :cost 0))
(defn effort [a] (get a :effort 0))
(defn duration [a] (get a :duration 1))
(defn- running [a] (get a :running 1))

(defn- first-day? [a] (= (running a) 1))
(defn- last-day? [a] (= (running a) (duration a)))

(defn- update-action [g old-action new-action]
  (-> g
      (update-in [:actions] disj old-action)
      (update-in [:actions] conj new-action)))

(defn- adj-level [level op val]
  (max (min 1.0 (op level val)) 0.0))

(defn- action-helper [func]
  (fn [g a]
    (-> g
        ;; Spend moneyz on the first day
        (cond-> (first-day? a)
                (update-in [:money] - (cost a)))

        ;; Run action
        (func a)

        (->/if (last-day? a)
          ;; Remove action on last day
          (remove-action a)

          ;; Count up running days
          (update-action a (assoc a :running (inc (running a))))))))

(def nop
  {:id :nop
   :desc "Keep playing"
   :effort 0
   :action (action-helper (fn [g a] g))})

(def surender
  {:id :surender
   :desc "Give up"
   :effort 0
   :action (action-helper
            (fn [g a]
              (-> g
                  (assoc-in [:status] :surender))))})

(def demo-template
  {:id :demo
   :action
   (fn [g a]
     (-> g
         (update-in [:recruitable] + 2)
         (->/when (= (a :type) :antifa)
           (update-in [:fascists :power] adj-level - 0.01)
           (update-in [:fascists :activity] adj-level - 0.01))
         (->/when (= (a :type) :anticap)
           (update-in [:capitalists :power] adj-level - 0.1)
           (update-in [:capitalists :activity] adj-level - 0.01)
           (update-in [:political-climate] adj-level + 0.01))))})

(defn create-demo [type activists]
  (merge demo-template
         {:type type
          :effort activists
          :desc (cond
                 (= type :antifa)
                 "Organize Antifa demonstration"
                 (= type :anticap)
                 "Orgnaize anti-capitalist demonstration")}))

(def online-campaign
  {:id :online-campaign
   :desc "Start online campaign"
   :effort 2
   :duration 5
   :action
   (action-helper
    (fn [g a]
      (-> g
          ;; First day
          (->/when (first-day? a)
            (update-in [:activists] - 2)
            (update-in [:fascists :activity] adj-level - 0.01))

          ;; All days
          (update-in [:fascists :power] adj-level - 0.01)
          (update-in [:political-climate] adj-level + 0.01)
          (update-in [:recruitable] + 1))))})

(def party
  {:id :party
   :desc "Support party"
   :effort 5
   :cost 1000
   :action
   (action-helper
    (fn [g a]
      (-> g
          (update-in [:recruitable] + 1)
          (update-in [:money] + 5000))))})

(defn antifa-group-action [g institution]
  (update-in g [:fascists :activity] adj-level - 0.01))

(def start-antifa-group
  {:id :antifa-group
   :desc "Start an antifa group"
   :effort 5
   :action
   (action-helper
    (fn [g {activists :effort}]
      (-> g
          (update-in [:activists] - activists)
          (update-in [:institutions] conj
                     {:id :antifa-group
                      :desc "Antifa Group"
                      :activists activists
                      :action antifa-group-action}))))})

(def handout-flyers
  {:id :handout-flyers
   :desc "Handout flyers"
   :effort 1
   :cost 50
   :action
   (action-helper
    (fn [g a]
      (-> g
          (update-in [:revolutionary-potential] adj-level + 0.01))))})

(def posters
  {:id :posters
   :desc "Stick up posters"
   :effort 2
   :cost 100
   :duration 3
   :action
   (action-helper
    (fn [g a]
      (-> g
          (->/when (first-day? a)
            (update-in [:police-repression] adj-level + 0.01))

          ;; Every day
          (update-in [:political-climate] adj-level + 0.01)
          (update-in [:recruitable] + 1))))})

(def stickers
  {:id :stickers
   :desc "Stickers"
   :effort 2
   :cost 200
   :duration 4
   :action
   (action-helper
    (fn [g a]
      (-> g
          (->/when (first-day? a)
            (update-in [:police-repression] adj-level + 0.01))

          ;; Every day
          (update-in [:political-climate] adj-level + 0.01)
          (update-in [:recruitable] + 1))))})


(defn comunity-center-action [g institution]
  (update-in g [:political-climate] adj-level + 0.01))

(def start-comunity-center
  {:id :comunity-center
   :desc "Start a comunity center"
   :effort 5
   :cost 1000
   :action
   (action-helper
    (fn [g {activists :effort}]
      (-> g
          (update-in [:activists] - activists)
          (update-in [:institutions] conj
                     {:id :comunity-center
                      :desc "Comunity Center"
                      :activists activists
                      :action comunity-center-action}))))})


(def reclaim-party
  {:id :reclaim-party
   :desc "Reclaim the Streets party"
   :effort 20
   :action
   (action-helper
    (fn [g a]
      (-> g
          (update-in [:recruitable] + 5)
          (update-in [:police-repression] adj-level + 0.05))))})



(defn union-action [g institution]
  (-> g
      (update-in [:political-climate] adj-level + 0.001)
      (update-in [:organized-workforce] adj-level + 0.005)))

(def start-union
  {:id :start-union
   :desc "Start a Union"
   :effort 10
   :action
   (action-helper
    (fn [g {activists :effort}]
      (-> g
          (update-in [:activists] - activists)
          (update-in [:institutions] conj
                     {:id :union
                      :desc "Union"
                      :activists activists
                      :action union-action}))))})


(def revolution
  {:id :revolution
   :desc "Revolution"
   :effort 10
   :action
   (action-helper
    (fn [g a]
      (-> g
          (assoc-in [:status] :revolution))))})


(def strike)
(def occupy-university)
(def start-anticap-group)
(def start-adbusting-group)
(def start-paper)
(def start-book-cafe)
(def start-comunity-center)
(def build-printshop)
(def occupy-abandoned-building)
(def start-hackerspace)
(def start-activist-food-truck)

(def general-strike)

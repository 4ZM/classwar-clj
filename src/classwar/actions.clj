(ns classwar.actions
  (:require [classwar.op :as cwo]
            [classwar.state :as cws]
            [lonocloud.synthread :as ->]))

(defmacro def-action [name desc params func]
  `(def ~name
     ~(merge {:id (keyword name)
              :type :action
              :desc desc
              :op `(cwo/op-helper ~func)}
             params)))

(def-action nop-action
  "Keep playing"
  {}
  (fn [g a] g))

(def-action surender
  "Give up"
  {}
  (fn [g a]
    (-> g
        (assoc-in [:status] :surender))))

(def-action antifa-demo
  "Organize antifa demonstration"
  {:effort 5}
  (fn [g a]
    (-> g
        (update-in [:recruitable] + 3.0)

        (update-in [:fascists :power] cwo/adj-level - 0.02)
        (->/if (cws/has-institution? :antifa-group)
          (update-in [:fascists :activity] cwo/adj-level - 0.02)
          (update-in [:fascists :activity] cwo/adj-level - 0.01)))))

(def-action anticap-demo
  "Orgnaize anti capitalist demonstration"
  {:effort 5}
  (fn [g a]
    (-> g
        (update-in [:recruitable] + 3.0)
        (update-in [:capitalists :power] cwo/adj-level - 0.02)
        (update-in [:capitalists :activity] cwo/adj-level - 0.01)
        (update-in [:political-climate] cwo/adj-level + 0.02))))

(def-action online-antifa-campaign
  "Start online antifa campaign"
  {:effort 2
   :duration 5}
  (fn [g a]
    (-> g
        ;; First day
        (->/when (cwo/first-day? a)
          (update-in [:fascists :activity] cwo/adj-level - 0.01)
          (update-in [:digest] conj
                     "You start an online antifa campaign and get some recruits"))

        ;; All days
        (update-in [:fascists :power] cwo/adj-level - 0.01)
        (update-in [:political-climate] cwo/adj-level + 0.01)
        (update-in [:recruitable] + 1.0))))

(def-action party
  "Support party"
  {:effort 5
   :cost 1000}
  (fn [g a]
    (-> g
        (update-in [:recruitable] + 1.0)
        (update-in [:money] + 5000))))

(defn antifa-group-action [g institution]
  (update-in g [:fascists :activity] cwo/adj-level - 0.01))

(def antifa-group
  {:id :antifa-group
   :type :institution
   :desc "Antifa Group"
   :action antifa-group-action})

(def-action start-antifa-group
  "Start an antifa group"
  {:effort 5}
  (fn [g {activists :effort}]
    (-> g
        (update-in [:activists] - activists)
        (update-in [:institutions] conj
                   (merge antifa-group
                          {:activists activists})))))

(def-action handout-flyers
  "Handout flyers"
  {:effort 1
   :cost 50}
  (fn [g a]
    (-> g
        (update-in [:capitalists :power] cwo/adj-level - 0.01)
        (update-in [:political-climate] cwo/adj-level + 0.01))))

(def-action posters
  "Stick up posters"
  {:effort 2
   :cost 100
   :duration 3}
  (fn [g a]
    (-> g
        (->/when (cwo/first-day? a)
          (update-in [:police-repression] cwo/adj-level + 0.01))

        ;; Every day
        (update-in [:political-climate] cwo/adj-level + 0.01)
        (update-in [:recruitable] + 0.5))))

(def-action stickers
  "Stickers"
  {:effort 2
   :cost 200
   :duration 4}
  (fn [g a]
    (-> g
        (->/when (cwo/first-day? a)
          (update-in [:police-repression] cwo/adj-level + 0.01))

        ;; Every day
        (update-in [:political-climate] cwo/adj-level + 0.01)
        (update-in [:recruitable] + 0.3))))


(defn comunity-center-action [g institution]
  (update-in g [:political-climate] cwo/adj-level + 0.01))

(def comunity-center
  {:id :comunity-center
   :type :institution
   :desc "Comunity Center"
   :activist-capacity 30
   :op comunity-center-action})

(def-action start-comunity-center
  "Start a comunity center"
  {:effort 5
   :cost 1000}
  (fn [g {activists :effort}]
    (-> g
        (update-in [:activists] - activists)
        (update-in [:institutions] conj
                   (merge {:activists activists}
                          comunity-center)))))


(def-action reclaim-party
  "Reclaim the Streets party"
  {:effort 20}
  (fn [g a]
    (-> g
        (update-in [:recruitable] + 5.0)
        (update-in [:police-repression] cwo/adj-level + 0.05))))

(defn union-action [g institution]
  (-> g
      (update-in [:political-climate] cwo/adj-level + 0.001)
      (update-in [:organized-workforce] cwo/adj-level + 0.005)))

(def union
  {:id :union
   :type :institution
   :desc "Union"
   :op union-action})


(def-action start-union
  "Start a Union"
  {:effort 10}
  (fn [g {activists :effort}]
    (-> g
        (update-in [:activists] - activists)
        (update-in [:institutions] conj
                   (merge {:activists activists} union)))))


(def-action revolution
  "Start Revolution"
  {:effort 10}
  (fn [g a]
    (-> g
        (assoc-in [:status] :revolution))))

(defn occupied-building-action [g a]
  (update-in g [:political-climate] cwo/adj-level + 0.01))

(def occupied-building
  {:id :occupied-building
   :type :institution
   :desc "Occupied Building"
   :activist-capacity 20
   :op occupied-building-action})

(def-action occupy-building
  "Occupy abandoned building"
  {:effort 5}
  (fn [g {activists :effort}]
    (-> g
        (update-in [:activists] - activists)
        (update-in [:institutions] conj
                   (merge {:activists activists}
                          occupied-building)))))

(def-action tear-down-fascist-propaganda
  "Tear down fascist propaganda"
  {:effort 3}
  (fn [g a]
    (let [posters (filter #(= (% :id) :fascist-posters) (cws/running-events g))]
      (-> g
          (update-in [:operations] (partial apply disj) posters)))))



(def strike)
(def occupy-university)
(def start-anticap-group)
(def start-adbusting-group)
(def start-paper)
(def start-book-cafe)
(def build-printshop)

(def start-hackerspace)
(def start-activist-food-truck)
(def create-web-site)

(def general-strike)

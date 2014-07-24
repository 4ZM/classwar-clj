(ns classwar.actions
  (:require [classwar.op :as cwo]
            [lonocloud.synthread :as ->]))

(def nop-action
  {:id :nop
   :type :action
   :desc "Keep playing"
   :effort 0
   :action (cwo/op-helper (fn [g a] g))})

(def surender
  {:id :surender
   :type :action
   :desc "Give up"
   :effort 0
   :action (cwo/op-helper
            (fn [g a]
              (-> g
                  (assoc-in [:status] :surender))))})

(def antifa-demo
  {:id :antifa-demo
   :type :action
   :desc "Organize antifa demonstration"
   :effort 5
   :action
   (cwo/op-helper
    (fn [g a]
      (-> g
          (update-in [:recruitable] + 2)
          (update-in [:fascists :power] cwo/adj-level - 0.02)
          (update-in [:fascists :activity] cwo/adj-level - 0.01))))})

(def anticap-demo
  {:id :anticap-demo
   :type :action
   :desc "Orgnaize anti capitalist demonstration"
   :effort 5
   :action
   (cwo/op-helper
    (fn [g a]
      (-> g
          (update-in [:recruitable] + 2)
          (update-in [:capitalists :power] cwo/adj-level - 0.02)
          (update-in [:capitalists :activity] cwo/adj-level - 0.01)
          (update-in [:political-climate] cwo/adj-level + 0.01))))})

(def online-campaign
  {:id :online-campaign
   :type :action
   :desc "Start online campaign"
   :effort 2
   :duration 5
   :action
   (cwo/op-helper
    (fn [g a]
      (-> g
          ;; First day
          (->/when (cwo/first-day? a)
            (update-in [:fascists :activity] cwo/adj-level - 0.01)
            (update-in [:digest] conj "You start an online campaign and get some recruits"))

          ;; All days
          (update-in [:fascists :power] cwo/adj-level - 0.01)
          (update-in [:political-climate] cwo/adj-level + 0.01)
          (update-in [:recruitable] + 1))))})

(def party
  {:id :party
   :type :action
   :desc "Support party"
   :effort 5
   :cost 1000
   :action
   (cwo/op-helper
    (fn [g a]
      (-> g
          (update-in [:recruitable] + 1)
          (update-in [:money] + 5000))))})

(defn antifa-group-action [g institution]
  (update-in g [:fascists :activity] cwo/adj-level - 0.01))

(def antifa-group
  {:id :antifa-group
   :type :institution
   :desc "Antifa Group"
   :action antifa-group-action})

(def start-antifa-group
  {:id :antifa-group
   :type :action
   :desc "Start an antifa group"
   :effort 5
   :action
   (cwo/op-helper
    (fn [g {activists :effort}]
      (-> g
          (update-in [:activists] - activists)
          (update-in [:institutions] conj
                     (merge antifa-group
                            {:activists activists})))))})

(def handout-flyers
  {:id :handout-flyers
   :type :action
   :desc "Handout flyers"
   :effort 1
   :cost 50
   :action
   (cwo/op-helper
    (fn [g a]
      (-> g
          (update-in [:political-climate] cwo/adj-level + 0.01))))})

(def posters
  {:id :posters
   :type :action
   :desc "Stick up posters"
   :effort 2
   :cost 100
   :duration 3
   :action
   (cwo/op-helper
    (fn [g a]
      (-> g
          (->/when (cwo/first-day? a)
            (update-in [:police-repression] cwo/adj-level + 0.01))

          ;; Every day
          (update-in [:political-climate] cwo/adj-level + 0.01)
          (update-in [:recruitable] + 1))))})

(def stickers
  {:id :stickers
   :type :action
   :desc "Stickers"
   :effort 2
   :cost 200
   :duration 4
   :action
   (cwo/op-helper
    (fn [g a]
      (-> g
          (->/when (cwo/first-day? a)
            (update-in [:police-repression] cwo/adj-level + 0.01))

          ;; Every day
          (update-in [:political-climate] cwo/adj-level + 0.01)
          (update-in [:recruitable] + 1))))})


(defn comunity-center-action [g institution]
  (update-in g [:political-climate] cwo/adj-level + 0.01))

(def comunity-center
  {:id :comunity-center
   :type :institution
   :desc "Comunity Center"
   :activist-capacity 30
   :action comunity-center-action})

(def start-comunity-center
  {:id :comunity-center
   :type :action
   :desc "Start a comunity center"
   :effort 5
   :cost 1000
   :action
   (cwo/op-helper
    (fn [g {activists :effort}]
      (-> g
          (update-in [:activists] - activists)
          (update-in [:institutions] conj
                     (merge {:activists activists}
                            comunity-center)))))})


(def reclaim-party
  {:id :reclaim-party
   :type :action
   :desc "Reclaim the Streets party"
   :effort 20
   :action
   (cwo/op-helper
    (fn [g a]
      (-> g
          (update-in [:recruitable] + 5)
          (update-in [:police-repression] cwo/adj-level + 0.05))))})



(defn union-action [g institution]
  (-> g
      (update-in [:political-climate] cwo/adj-level + 0.001)
      (update-in [:organized-workforce] cwo/adj-level + 0.005)))

(def union
  {:id :union
   :type :institution
   :desc "Union"
   :action union-action})


(def start-union
  {:id :start-union
   :type :action
   :desc "Start a Union"
   :effort 10
   :action
   (cwo/op-helper
    (fn [g {activists :effort}]
      (-> g
          (update-in [:activists] - activists)
          (update-in [:institutions] conj
                     (merge {:activists activists} union)))))})


(def revolution
  {:id :revolution
   :type :action
   :desc "Revolution"
   :effort 10
   :action
   (cwo/op-helper
    (fn [g a]
      (-> g
          (assoc-in [:status] :revolution))))})

(defn occupied-building-action [g a]
  (update-in g [:political-climate] cwo/adj-level + 0.01))

(def occupied-building
  {:id :occupied-building
   :type :institution
   :desc "Occupied Building"
   :activist-capacity 20
   :action occupied-building-action})

(def occupy-building
  {:id :occupy-building
   :type :action
   :desc "Occupy abandoned building"
   :effort 5
   :action
   (cwo/op-helper
    (fn [g {activists :effort}]
      (-> g
          (update-in [:activists] - activists)
          (update-in [:institutions] conj
                     (merge {:activists activists}
                            occupied-building)))))})

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
(def tear-down-propaganda)
(def general-strike)

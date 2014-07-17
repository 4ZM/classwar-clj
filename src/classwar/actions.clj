(ns classwar.actions
  (:require [lonocloud.synthread :as ->]))

(def nop
  {:id :nop
   :desc "Keep playing"
   :effort 0
   :action (fn [g _] g)})

(def surender
  {:id :surender
   :desc "Give up"
   :duration 0
   :effort 0
   :action (fn [g _] (assoc-in g [:status] :surender))})

(defn adj-level [level op val]
  (max (min 1.0 (op level val)) 0.0))

(def demo-template
  {:id :demo
   :action
   (fn [g a]
     (-> g
         (update-in [:prospects] + 2)
         (->/when (= (a :type) :antifa)
           (update-in [:fascists :power] adj-level - 0.01)
           (update-in [:fascists :activity] adj-level - 0.01))
         (->/when (= (a :type) :anticap)
           (update-in [:capitalists :power] adj-level - 0.1)
           (update-in [:capitalists :activity] adj-level - 0.01)
           (update-in [:political-climate] - 0.01))))})

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
   :action
   (fn [g _]
     (-> g
         (update-in [:prospects] + 1)
         (update-in [:fascists :activity] adj-level - 0.01)
         (update-in [:political-climate] - 0.01)))})

(def party
  {:id :party
   :desc "Support party"
   :effort 5
   :cost 1000
   :action
   (fn [g _]
     (-> g
         (update-in [:prospects] + 1)
         (update-in [:money] + 5000)))})

(defn antifa-group-action [g institution]
  (update-in g [:fascists :activity] adj-level - 0.01))

(def start-antifa-group
  {:id :antifa-group
   :desc "Start an antifa group"
   :effort 5
   :action
   (fn [g a]
     (let [req-activists (a :effort)]
       (-> g
           (update-in [:activists] - req-activists)
           (update-in [:institutions] conj
                      {:id :antifa-group
                       :desc "Antifa Group"
                       :activists req-activists
                       :action antifa-group-action}))))})

(def handout-flyers
  {:id :handout-flyers
   :desc "Handout flyers"
   :effort 1
   :cost 50
   :action
   (fn [g _]
     (-> g
         (update-in [:revolutionary-potential] adj-level + 0.01)))})

(def posters
  {:id :posters
   :desc "Stick up posters"
   :effort 2
   :cost 100
   :action
   (fn [g _]
     (-> g
         (update-in [:police-repression] adj-level + 0.01)))})

(def stickers
  {:id :stickers
   :desc "Stickers"
   :effort 2
   :cost 200
   :action
   (fn [g _]
     (-> g
         (update-in [:police-repression] adj-level + 0.01)))})


(defn comunity-center-action [g institution]
  (update-in g [:political-climate] - 0.01))

(def start-comunity-center
  {:id :comunity-center
   :desc "Start a comunity center"
   :effort 5
   :cost 1000
   :action
   (fn [g a]
     (let [req-activists (a :effort)]
       (-> g
           (update-in [:activists] - req-activists)
           (update-in [:institutions] conj
                      {:id :comunity-center
                       :desc "Comunity Center"
                       :activists req-activists
                       :action comunity-center-action}))))})


(def reclaim-party
  {:id :reclaim-party
   :desc "Reclaim the Streets party"
   :effort 20
   :action
   (fn [g _]
     (-> g
         (update-in [:prospects] + 5)
         (update-in [:police-repression] adj-level + 0.05)))})



(defn union-action [g institution]
  (-> g
      (update-in [:political-climate] - 0.001)
      (update-in [:organized-workforce] + 0.005)))

(def start-union
  {:id :start-union
   :desc "Start a Union"
   :effort 10
   :action
   (fn [g a]
     (let [req-activists (a :effort)]
       (-> g
           (update-in [:activists] - req-activists)
           (update-in [:institutions] conj
                      {:id :union
                       :desc "Union"
                       :activists req-activists
                       :action union-action}))))})


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

(ns wanna-bet.core
  (:require
   [reagent.core :as reagent :refer [atom]]
   [ajax.core :as ajx]
   [reagent.session :as session]
   [reitit.frontend :as reitit]
   [clerk.core :as clerk]
   [accountant.core :as accountant]))

;; -------------------------
;; Routes

(def router
  (reitit/router
   [["/" :index]
    ["/items"
     ["" :items]
     ["/:item-id" :item]]
    ["/about" :about]]))

(defn path-for [route & [params]]
  (if params
    (:path (reitit/match-by-name router route params))
    (:path (reitit/match-by-name router route))))

(path-for :about)

;; -------------------------
;; State

(defonce state
  (atom {:phone 22
                 :search ""
                 :order-prop :name}))

(defn load-phones! 
  "Fetches the list of phones from the server and updates the state atom with it"
  [state]
  (ajx/GET "/rand"
    {:handler (fn [phone] (swap! state assoc :phone (:phone phone)))
     :error-handler (fn [details] (.warn js/console (str "Failed to refresh phones from server: " details)))
     :response-format :json, :keywords? true}))

;; -------------------------
;; Page components

(defn home-page []
  (do (load-phones! state)
      (fn []
        (let [phone (:phone @state)]
          [:span.main
           [:h1 (str "Wanna bet? " phone)]
           [:ul
            [:li [:a {:href (path-for :items)} "Items of wanna-bet"]]
            [:li [:a {:href "/borken/link"} "Borken link"]]]])
        )))

(defn items-page []
  (fn []
    [:span.main
     [:h1 "The items of wanna-bet"]
     [:ul (map (fn [item-id]
                 [:li {:name (str "item-" item-id) :key (str "item-" item-id)}
                  [:a {:href (path-for :item {:item-id item-id})} "Item: " item-id]])
               (range 1 60))]]))

(defn item-page []
  (fn []
    (let [routing-data (session/get :route)
          item (get-in routing-data [:route-params :item-id])]
      [:span.main
       [:h1 (str "Item " item " of wanna-bet")]
       [:p [:a {:href (path-for :items)} "Back to the list of items"]]])))

(defn about-page []
  (fn [] [:span.main
          [:h1 "About wanna-bet"]]))

;; -------------------------
;; Translate routes -> page components

(defn page-for [route]
  (case route
    :index #'home-page
    :about #'about-page
    :items #'items-page
    :item #'item-page))


;; -------------------------
;; Page mounting component

(defn current-page []
  (fn []
    (let [page (:current-page (session/get :route))]
      [:div
       [:header
        [:p [:a {:href (path-for :index)} "Home"] " | "
         [:a {:href (path-for :about)} "About wanna-bet"]]]
       [page]
       [:footer
        [:p "wanna-bet was generated by the "
         [:a {:href "https://github.com/reagent-project/reagent-template"} "Reagent Template"] "."]]])))

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (clerk/initialize!)
  (accountant/configure-navigation!
   {:nav-handler
    (fn [path]
      (let [match (reitit/match-by-path router path)
            current-page (:name (:data  match))
            route-params (:path-params match)]
        (reagent/after-render clerk/after-render!)
        (session/put! :route {:current-page (page-for current-page)
                              :route-params route-params})
        (clerk/navigate-page! path)
        ))
    :path-exists?
    (fn [path]
      (boolean (reitit/match-by-path router path)))})
  (accountant/dispatch-current!)
  (load-phones! state)
  (mount-root))

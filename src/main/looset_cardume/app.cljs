(ns looset-cardume.app
  (:require
    [reagent.dom]
    [reagent.core :as reagent]
    ["mermaid" :as mermaid]
    [re-frame.core :as re-frame]))

(when ^boolean js/goog.DEBUG ;; Code removed in production
  (js/console.log "Debugger mode!"))

;; Redef re-frame subscribe and dispatch for brevity
(def <sub (comp deref re-frame.core/subscribe))
(def >evt re-frame.core/dispatch)

(defn with-mount-fn
  "Wrap component in the create-class fn so the react component-did-mount
  hook can be used."
  [[_n {:keys [component-did-mount]} :as to-render]]
  (reagent/create-class
    {:reagent-render #(into [] (update-in to-render [1]
                                          dissoc :component-did-mount))
     :component-did-mount component-did-mount}))

(defn sequence-1
  [app-state]
  (get-in app-state [:domain :sequence-1] ""))
(re-frame/reg-sub ::sequence-1 sequence-1)

(defn set-sequence-1
  [app-state [_event v]]
  (assoc-in app-state [:domain :sequence-1] v))
  ;; (.contentLoaded mermaid))
(re-frame/reg-event-db ::set-sequence-1 set-sequence-1)

(def initial-state
  {:domain {:sequence-1 "sequenceDiagram\nA->>B: B\n"}
   :ui {}})

(defn initialize-mermaid []
  (.initialize mermaid
    #js {:theme "forest"}))

(defn my-elem []
  [:<>
   [:h1 "Hello!"]
   [:div
    [(with-mount-fn
       [:div#sequence-1.mermaid
        {:component-did-mount #(.contentLoaded mermaid)}
        (<sub [::sequence-1])])]]
   [:textArea
    {:onChange #(>evt [::set-sequence-1 (-> % .-target .-value)])}
    (<sub [::sequence-1])]])

(re-frame/reg-event-db ::set-app-state
  (fn [_ [_event application-state]]
    application-state))

(defn init-state []
  (re-frame/dispatch-sync [::set-app-state initial-state]))

(defn ^:dev/after-load mount-app-element []
  (when ^boolean js/goog.DEBUG ;; Code removed in production
    ;; (day8.re-frame-10x/show-panel! false)
    (re-frame/clear-subscription-cache!))
  (when-let [el (.getElementById js/document "root")]
    (reagent.dom/render [my-elem] el)))
  ;; (.contentLoaded mermaid))

(defn init []
  (init-state)
  (initialize-mermaid)
  (mount-app-element))

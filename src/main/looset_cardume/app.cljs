(ns looset-cardume.app
  (:require
    [reagent.dom]
    [re-frame.core :as re-frame]))

;; Redef re-frame subscribe and dispatch for brevity
(def <sub (comp deref re-frame.core/subscribe))
(def >evt re-frame.core/dispatch)

(defn my-elem []
  [:h1 "Hello!"
   [:button
    {:onClick #(js/alert "x")}
    "bla"]])

(defn ^:dev/after-load init []
  (when-let [el (.getElementById js/document "root")]
    (reagent.dom/render [my-elem] el)))

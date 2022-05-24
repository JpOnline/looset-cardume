(ns looset-cardume.app
  (:require
    [clojure.string :as str]
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

(defn cardume-text
  [app-state]
  (get-in app-state [:domain :cardume-text] ""))
(re-frame/reg-sub ::cardume-text cardume-text)

(defn modes
  [app-state]
  (-> app-state
    (get-in [:ui :modes] [])
    (->> (map #(assoc % :checked? (= (:id %)
                                     (get-in app-state [:ui :selected-mode] "Cardume")))))))
(re-frame/reg-sub ::modes modes)

(defn selected-mode
  [app-state]
  (get-in app-state [:ui :selected-mode] "Cardume"))
(re-frame/reg-sub ::selected-mode selected-mode)

(defn process-foldings [lines]
  (first
    (reduce
      (fn [[r folding?] line]
        (let [[_ fold-replacement :as start-folding?](re-find #"start-fold (.*)" line)
              end-folding? (re-find #"end-fold" line)]
          (cond
            (and (not folding?) start-folding?)
            [(conj r fold-replacement) true]

            (not folding?)
            [(conj r line) false]

            end-folding?
            [r false]

            :else ;; still-folding
            [r true])))
      [[] false] lines)))

(defn mermaid-text
  [cardume-text]
  (as-> cardume-text $
    (str/split $ #"\n")
    (process-foldings $)
    (str/join "\n" $)))
(re-frame/reg-sub
  ::mermaid-text
  :<- [::cardume-text]
  mermaid-text)

(defn line-data [line]
  (let [actor-pattern #"([^\+\->:\n,;\"participant\"%]+)((?!(\-x|\-\-x|\-\)|\-\-\)))[\-]*[^\+\->:\n,;]+)*"
        signal-type-pattern #"->>|-->>|->|-->|\-[x]|\-\-[x]|\-[\)]|\-\-[\)]"
        final-pattern (re-pattern (str actor-pattern signal-type-pattern))]
    (str line (re-find final-pattern line))))

(defn cardume-view
  [cardume-text]
  (map line-data (str/split cardume-text #"\n")))
(re-frame/reg-sub
  ::cardume-view
  :<- [::cardume-text]
  cardume-view)


(defn set-cardume-text
  [app-state [_event v]]
  (assoc-in app-state [:domain :cardume-text] v))
(re-frame/reg-event-db ::set-cardume-text set-cardume-text)

(defn select-mode
  [app-state [_event v]]
  (assoc-in app-state [:ui :selected-mode] v))
(re-frame/reg-event-db ::select-mode select-mode)

(def initial-state
  {:domain {:cardume-text "sequenceDiagram\nparticipant A as Aliased A\nA->>B: a\n% start-fold A->>B: bc\nB->>A: b\nA-->B: c\n% end-fold\nB->>B: d"}
   :ui {:selected-mode "Cardume"
        :modes [{:id "Cardume"}
                {:id "Cardume Text"}
                {:id "Mermaid Text"}]}})

(defn initialize-mermaid []
  (.initialize mermaid
    #js {:theme "forest"}))

(defn cardume []
  [:<>
    (map
      (fn [line]
        ^{:key line}
        [:pre line])
      (<sub [::cardume-view]))])

(defn my-elem []
  [:<>
   [:h1 "Looset Cardume"]
   [:div
    "Mode:"
    (map (fn [{:keys [id checked?]}]
           ^{:key id}
           [:<>
            [:input {:checked checked?
                     :onClick #(>evt [::select-mode id])
                     :type "radio"
                     :id id
                     :name "mode"
                     :value id}]
            [:label {:for id} id]])
         (<sub [::modes]))]
   [:textarea
    {:style {:height 200 :width 400}
     :onChange #(>evt [::set-cardume-text (-> % .-target .-value)])
     :value (<sub [::cardume-text])}]
   (case (<sub [::selected-mode])
     "Cardume" [cardume]
     "Cardume Text" [:pre (<sub [::cardume-text])]
     "Mermaid Text" [:pre (<sub [::mermaid-text])])
   [:div
    [(with-mount-fn
       [:div#sequence-1.mermaid
        {:component-did-mount #(.contentLoaded mermaid)}
        (<sub [::mermaid-text])])]]])

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
    (reagent.dom/render [my-elem] el))
  (.contentLoaded mermaid))

(defn init []
  (init-state)
  (initialize-mermaid)
  (mount-app-element))

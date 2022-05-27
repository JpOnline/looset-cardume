(ns looset-cardume.app
  (:require
    [clojure.string :as str]
    [reagent.dom]
    [reagent.core :as reagent]
    ["mermaid" :as mermaid]
    [re-frame.core :as re-frame]
    ["dragselect" :as dragselect]))

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

;; ---- SUBS ----

(defn valid-cardume-text
  [app-state]
  (get-in app-state [:ui :validation :valid-cardume-text] ""))
(re-frame/reg-sub ::valid-cardume-text valid-cardume-text)

(defn modes
  [app-state]
  (-> app-state
    (get-in [:ui :mode :items] [])
    (->> (map #(assoc % :checked? (= (:id %)
                                     (get-in app-state [:ui :mode :selected] "Cardume")))))))
(re-frame/reg-sub ::modes modes)

(defn selected-mode
  [app-state]
  (get-in app-state [:ui :mode :selected] "Cardume"))
(re-frame/reg-sub ::selected-mode selected-mode)

(defn process-foldings [lines]
  (first
    (reduce
      (fn [[r folding?] line]
        (let [[_ _fold-status fold-replacement :as start-folding?](re-find #"(\w+)-fold (.*)" line)
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
  :<- [::valid-cardume-text]
  mermaid-text)

(defn process-foldings2 [data-lines]
  (first
    (reduce
      (fn [[res folding-level [fold-status & folding-stack]] {:keys [text] :as data-line}]
        (let [[_ new-fold-status fold-replacement :as start-folding?](re-find #"(\w+)-fold (.*)" text)
              end-folding? (re-find #"end-fold" text)]
          (cond
            start-folding?
            [(conj res (-> data-line
                         (assoc :fold-level folding-level)
                         (assoc :replacement-text fold-replacement)
                         (assoc :fold-status new-fold-status)))
             (inc folding-level)
             (conj folding-stack fold-status new-fold-status)]

            end-folding?
            [(conj res (-> data-line
                         (assoc :fold-status fold-status)
                         (assoc :fold-level folding-level)))
             (dec folding-level)
             folding-stack]

            :else
            [(conj res (-> data-line
                         (assoc :fold-status fold-status)
                         (assoc :fold-level folding-level)))
             folding-level
             (conj folding-stack fold-status)])))
      [[] 0 '()] data-lines)))

(defn cardume-view
  [cardume-text]
  ;; (let [mermaid-meta (try (.-yy (.-parser (.parse mermaid cardume-text)))
  ;;                         (catch :default _
  ;;                           ""))
  ;;       actors (.getActors mermaid-meta)
  ;;       calls (.getMessages mermaid-meta)]
  ;;   (js/console.log "mermaid" mermaid-meta)
  (->> (str/split cardume-text #"\n")
    (map #(into {:text %}))
    (process-foldings2)))
(re-frame/reg-sub
  ::cardume-view
  :<- [::cardume-text]
  cardume-view)

(defn valid-diagram?
  [app-state]
  (get-in app-state [:ui :validation :valid-diagram?] false))
(re-frame/reg-sub ::valid-diagram? valid-diagram?)

(defn selected-lines
  [app-state]
  (get-in app-state [:ui :cardume :selected-lines] {}))
(re-frame/reg-sub ::selected-lines selected-lines)

;; ---- EVENTS ----

(defn trim-lines [txt]
  (->> (str/split txt #"\n")
    (map str/trim)
    (str/join "\n")))

(defn set-cardume-text
  [app-state [_event v]]
  (try (.parse mermaid (trim-lines v))
       (-> app-state
         (assoc-in [:domain :cardume-text] v)
         (assoc-in [:ui :validation :valid-cardume-text] v)
         (assoc-in [:ui :validation :valid-diagram?] true))
       (catch :default _
         (-> app-state
           (assoc-in [:domain :cardume-text] v)
           (assoc-in [:ui :validation :valid-diagram?] false)))))
(re-frame/reg-event-db ::set-cardume-text set-cardume-text)

(defn select-mode
  [app-state [_event v]]
  (assoc-in app-state [:ui :mode :selected] v))
(re-frame/reg-event-db ::select-mode select-mode)

(defn set-selected-line
  [app-state [_event line selected?]]
  (js/console.log "set-sel" line selected?)
  (assoc-in app-state [:ui :cardume :selected-lines line] selected?))
(re-frame/reg-event-db ::set-selected-line set-selected-line)

(defn toggle-cardume
  [app-state [_event line-number new-state]]
  (let [_ (js/console.log "jp 0" line-number new-state)
        state-cardume-text (cardume-text app-state)
        _ (js/console.log "jp 1" state-cardume-text)
        lines (str/split state-cardume-text #"\n")
        updated-lines (update lines line-number str/replace #"open|closed" new-state)]
    (assoc-in app-state [:domain :cardume-text] (str/join "\n" updated-lines))))
(re-frame/reg-event-db ::toggle-cardume toggle-cardume)

;; ---- Views ----

(declare initialize-dragselect)

(defn cardume []
  [:<>
   [:style
    "
    .ds-selected {
      background-color: lightgray;
    }
    "]
   [(with-mount-fn
      [:div#cardume
       {:style {:user-select "none"
                :width "400px"}
        :component-did-mount initialize-dragselect}
       (map-indexed
         (fn [idx {:keys [text fold-level replacement-text fold-status]}]
           ^{:key text}
           [:pre.selectable
            {:id idx
             :onDoubleClick (when replacement-text #(js/alert idx))
             :style {:margin "0"
                     :padding "0 10px"
                     :padding-left (str (+ 10 (* 15 fold-level))"px")}}
            (cond
              (and replacement-text (= "open" fold-status))
              [:<> [:b {:onClick #(>evt [::toggle-cardume idx "closed"])} "v "] replacement-text]

              (and replacement-text (= "closed" fold-status))
              [:<> [:b {:onClick #(>evt [::toggle-cardume idx "open"])}  "> "] replacement-text]

              (= "closed" fold-status)
              ""

              :else
              text)])
         (<sub [::cardume-view]))])]])

(defn data-lines []
  [:div
   (map-indexed
     (fn [idx line]
       ^{:key line}
       [:pre.selectable
        {:id idx
         :style {:margin "0"
                 :padding "2px 0px"}}
        line])
     (<sub [::cardume-view]))])

(defn diagram-comp []
  (let [mermaid-text (<sub [::mermaid-text])
        valid-diagram? (<sub [::valid-diagram?])]
    [(with-mount-fn
       [:div#sequence-1.mermaid
        {:component-did-mount #(.contentLoaded mermaid)
         :style {:opacity (if valid-diagram? "100%" "40%")}}
        mermaid-text])]))

(defn main []
  [:<>
   [:h1 "Looset Cardume"]
   [:div
    "Mode:"
    (map (fn [{:keys [id checked?]}]
           ^{:key id}
           [:<>
            [:input {:checked checked?
                     :onChange #(>evt [::select-mode id])
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
   [data-lines]
   (case (<sub [::selected-mode])
     "Cardume" [cardume]
     "Cardume Text" [:pre (<sub [::cardume-text])]
     "Mermaid Text" [:pre (<sub [::mermaid-text])])
   [diagram-comp]])

;; ---- Initialization ----

(defn initialize-dragselect []
  (let [ds (dragselect.
             #js {:selectables (js/document.getElementsByClassName "selectable")
                  :draggability false
                  :area (js/document.getElementById "cardume")})]
    (.subscribe ds "elementselect" (fn [e]
                                     (>evt [::set-selected-line (.-id (.-item e)) true])
                                     (js/console.log "selected" (clj->js (.-id (.-item e))))))
    (.subscribe ds "elementunselect" (fn [e] (js/console.log "unselected" (clj->js (.-id (.-item e))))))
    (.subscribe ds "callback" (fn [e] (js/console.log "mouse up" (clj->js (map (fn [i] (.-id i)) (.-items e))))))))

(def initial-state
  {:domain {:cardume-text "sequenceDiagram\nparticipant A as Aliased A\nA->>B: a\n% open-fold A->>B: bc\nB->>A: b\nA-->B: c\n% end-fold\nB->>B: d"}
   :ui {:cardume {:selected-lines {}}
        :mode {:selected "Cardume"
               :items [{:id "Cardume"}
                       {:id "Cardume Text"}
                       {:id "Mermaid Text"}]}}})

(defn initialize-mermaid []
  (.initialize mermaid
    #js {:theme "forest"}))

(re-frame/reg-event-db ::set-app-state
  (fn [_ [_event application-state]]
    (-> application-state
      (assoc-in [:ui :validation :valid-diagram?] true)
      (assoc-in [:ui :validation :valid-cardume-text] (get-in application-state [:domain :cardume-text])))))

(defn init-state []
  (re-frame/dispatch-sync [::set-app-state initial-state]))

(defn ^:dev/after-load mount-app-element []
  (when ^boolean js/goog.DEBUG ;; Code removed in production
    ;; (day8.re-frame-10x/show-panel! false)
    (re-frame/clear-subscription-cache!))
  (when-let [el (.getElementById js/document "root")]
    (reagent.dom/render [main] el))
  (.contentLoaded mermaid))

(defn init []
  (init-state)
  (initialize-mermaid)
  (mount-app-element))

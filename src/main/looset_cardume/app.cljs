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
      (fn [[r folding-level] {:keys [text] :as data-line}]
        (let [[_ fold-status fold-replacement :as start-folding?](re-find #"(\w+)-fold (.*)" text)
              end-folding? (re-find #"end-fold" text)]
          (cond
            start-folding?
            [(conj r (-> data-line
                       (assoc :fold-level (inc folding-level))
                       (assoc :replacement-text fold-replacement)
                       (assoc :fold-status fold-status)))
             (inc folding-level)]

            end-folding?
            [(conj r (assoc data-line :fold-level folding-level)) (dec folding-level)]

            :else
            [(conj r (assoc data-line :fold-level folding-level)) folding-level])))
      [[] 0] data-lines)))

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

(def initial-state
  {:domain {:cardume-text "sequenceDiagram\nparticipant A as Aliased A\nA->>B: a\n% open-fold A->>B: bc\nB->>A: b\nA-->B: c\n% end-fold\nB->>B: d"}
   :ui {:cardume {:selected []}
        :mode {:selected "Cardume"
               :items [{:id "Cardume"}
                       {:id "Cardume Text"}
                       {:id "Mermaid Text"}]}}})

(defn initialize-mermaid []
  (.initialize mermaid
    #js {:theme "forest"}))

(defn initialize-dragselect []
  (let [ds (dragselect.
             #js {:selectables (js/document.getElementsByClassName "selectable")
                  :draggability false
                  :area (js/document.getElementById "cardume")})]
    (.subscribe ds "callback" (fn [e] (js/console.log (clj->js (map (fn [i] (.-id i)) (.-items e))))))))


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
         (fn [idx {:keys [text fold-level replacement-text]}]
           ^{:key text}
           [:pre.selectable
            {:id idx
             :style {:margin "0"
                     :padding "0 10px"
                     :padding-left (str (+ 10 (* 15 fold-level))"px")}}
            (str (when replacement-text "> ") text)])
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

(defn my-elem []
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
    (reagent.dom/render [my-elem] el))
  (.contentLoaded mermaid))

(defn init []
  (init-state)
  (initialize-mermaid)
  (mount-app-element))

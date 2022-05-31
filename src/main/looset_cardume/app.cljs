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

(defonce drag-select (atom nil))

;; ---- SUBS ----

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

(defn process-mermaid [lines]
  (first
    (reduce
      (fn [[r folding?] line]
        (let [[_ fold-status fold-replacement] (re-find #"(\w+)-fold (.*)" line)
              end-folding? (re-find #"end-fold" line)]
          (cond
            (and (not folding?) (= fold-status "closed"))
            [(conj r fold-replacement) true]

            (and (not folding?) (= fold-status "open"))
            [r false]

            end-folding?
            [r false]

            (not folding?)
            [(conj r line) false]

            :else ;; still-folding
            [r true])))
      [[] false] lines)))

(defn mermaid-text
  [cardume-text]
  (as-> cardume-text $
    (str/split $ #"\n")
    (process-mermaid $)
    (str/join "\n" $)))
(re-frame/reg-sub
  ::mermaid-text
  :<- [::valid-cardume-text]
  mermaid-text)

(defn process-cardume [data-lines]
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
    (process-cardume)))
(re-frame/reg-sub
  ::cardume-view
  :<- [::cardume-text]
  cardume-view)

(defn cardume-line
  [cardume-view [_ line-num]]
  (nth cardume-view line-num))
(re-frame/reg-sub
  ::cardume-line
  :<- [::cardume-view]
  cardume-line)

(defn valid-diagram?
  [app-state]
  (get-in app-state [:ui :validation :valid-diagram?] false))
(re-frame/reg-sub ::valid-diagram? valid-diagram?)

(defn editing-line
  [app-state]
  (get-in app-state [:ui :cardume :editing-line] nil))
(re-frame/reg-sub ::editing-line editing-line)

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

(defn toggle-cardume
  [app-state [event line-number new-state]]
  (let [state-cardume-text (cardume-text app-state)
        lines (str/split state-cardume-text #"\n")
        updated-lines (update lines line-number str/replace #"open|closed" new-state)]
    (set-cardume-text app-state [event (str/join "\n" updated-lines)])))
(re-frame/reg-event-db ::toggle-cardume toggle-cardume)

(defn remove-folds-without-header
  [data-lines]
  (first
    (reduce
      (fn [[res removing?] {:keys [text replacement-text] :as data-line}]
        (cond
          (= "" replacement-text)
          [res true]

          (and removing? (re-find #"end-fold" text))
          [res false]

          :else
          [(conj res data-line) removing?]))
      [[] false] data-lines)))

(defn finish-line-edition
  [app-state [event]]
  (let [state-cardume-text (cardume-text app-state)
        new-cardume-text (->> (str/split state-cardume-text #"\n")
                           (map #(into {:text %}))
                           (process-cardume)
                           (remove-folds-without-header)
                           (map :text)
                           (str/join "\n"))]
    (.clearSelection ^js/dragselect @drag-select)
    (-> app-state
      (set-cardume-text [event new-cardume-text])
      (assoc-in [:ui :cardume :editing-line] nil))))
(re-frame/reg-event-db ::finish-line-edition finish-line-edition)

(defn insert-at [coll idx el]
  (let [[before after] (split-at idx coll)]
    (concat before [el] after)))

(re-frame/reg-fx
  :focus-to-element
  (fn [element-id]
    (reagent/after-render #(some-> (js/document.getElementById element-id) .focus))))

(defn lines-select-mouse-up
  [{app-state :db} [event selected-lines]]
  (let [updated-app-state (assoc-in app-state [:ui :cardume :selected-lines] selected-lines)
        num-selected (count selected-lines)
        sorted-selected (sort selected-lines)
        first-selected (js/parseInt (first sorted-selected))
        last-selected (js/parseInt (last sorted-selected))
        state-cardume-text (cardume-text app-state)
        lines (str/split state-cardume-text #"\n")
        new-lines (-> lines
                    (insert-at first-selected "% closed-fold ")
                    (insert-at (+ 2 last-selected) "% end-fold"))
        new-cardume-text (str/join "\n" new-lines)
        editing (get-in app-state [:ui :cardume :editing-line])]
    (if (or (< num-selected 2) editing)
      {:db updated-app-state}
      {:focus-to-element "input-line"
       :db (-> updated-app-state
             (assoc-in [:ui :cardume :editing-line] first-selected)
             (set-cardume-text [event new-cardume-text]))})))
(re-frame/reg-event-fx ::lines-select-mouse-up lines-select-mouse-up)

(defn set-cardume-line-text
  [app-state [event line-num original-text new-text]]
  (let [[_ folding-header] (re-find #"(.*\w+-fold )" original-text)
        state-cardume-text (cardume-text app-state)
        lines (str/split state-cardume-text #"\n")
        updated-lines (assoc lines line-num (str folding-header new-text))]
    (set-cardume-text app-state [event (str/join "\n" updated-lines)])))
(re-frame/reg-event-db ::set-cardume-line-text set-cardume-line-text)

(defn set-editing-line
  [{app-state :db} [_event line-num]]
  {:focus-to-element "input-line"
   :db (assoc-in app-state [:ui :cardume :editing-line] line-num)})
(re-frame/reg-event-fx ::set-editing-line set-editing-line)

;; ---- Views ----

(declare initialize-dragselect)

(defn cardume-line-pre-el [idx {:keys [text fold-level replacement-text fold-status]}]
  [:pre.selectable
   {:id idx
    :onDoubleClick #(>evt [::set-editing-line idx])
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

     (re-find #"end-fold" text)
     ""

     :else
     text)])

(defn cardume []
  (let [editing-line (js/parseInt (<sub [::editing-line]))]
    [:<>
     [:style
      "
      .ds-selected {
        background-color: lightgray;
      }
      "]
     [:div#cardume
      {:style {:user-select "none"
               :width "400px"}}
      (into [:<>]
        (for [idx (range (count (<sub [::cardume-view])))]
          (let [{:keys [text replacement-text] :as line-data} (<sub [::cardume-line idx])]
            (if (= editing-line idx)
              [:input#input-line.selectable
               {:onKeyPress #(when (= (.-key %) "Enter") (>evt [::finish-line-edition]))
                :onBlur #(>evt [::finish-line-edition])
                :value (or replacement-text text)
                :onChange #(>evt [::set-cardume-line-text idx text (-> % .-target .-value)])}]
              [cardume-line-pre-el idx line-data]))))]]))

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
                                     (.addSelectables ds (js/document.getElementsByClassName "selectable"))
                                     (js/console.log "selected" (clj->js (.-id (.-item e))))))
    (.subscribe ds "elementunselect" (fn [e]
                                       (js/console.log "unselected" (clj->js (.-id (.-item e))))))
    (.subscribe ds "callback" (fn [e]
                                (re-frame/dispatch-sync [::lines-select-mouse-up (mapv (fn [i] (.-id i)) (.-items e))])
                                ;; (when (<sub [::editing-line])
                                ;;   (.clearSelection ds))
                                (.addSelectables ds (js/document.getElementsByClassName "selectable"))
                                (js/console.log "mouse up!"
                                                (clj->js (map (fn [i] (.-id i)) (.-items e))))))
    ds))

(def initial-state
  {:domain {:cardume-text "sequenceDiagram\nparticipant A as Aliased A\nA->>B: a\n% open-fold A->>B: bc\nB->>A: b\nA-->B: c\n% end-fold\nB->>B: d"}
   :ui {:cardume {:selected-lines {}
                  :editing-line nil}
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
  (.contentLoaded mermaid)
  (reset! drag-select (initialize-dragselect)))

(defn init []
  (init-state)
  (initialize-mermaid)
  (mount-app-element))

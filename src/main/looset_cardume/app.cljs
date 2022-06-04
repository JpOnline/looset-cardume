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

(defn fold-header [s]
  (re-find #"(\w+)-fold (.*)" (or s "")))

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

(defn process-cardume [data-lines]
  (first
    (reduce
      (fn [[res idx [{:keys [fold-status starter-line] :as stack-item} & folding-stack]] {:keys [original-text] :as data-line}]
        (let [[_ extracted-fold-status replacement-text :as folding-head?] (fold-header original-text)
              end-folding? (re-find #"end-fold" original-text)
              new-fold-status (if (= "closed" fold-status) "closed" extracted-fold-status)
              new-folding-stack (cond
                                  folding-head? (conj folding-stack stack-item {:fold-status new-fold-status :starter-line idx})
                                  end-folding? folding-stack
                                  :else (conj folding-stack stack-item))
              [arrow-position toggle-event] (if (= "closed" extracted-fold-status)
                                              ["> " "open"]
                                              ["v " "closed"])
              show-fold-head? (and folding-head? (not= fold-status "closed"))
              text (cond
                     (= "closed" fold-status) ""
                     folding-head? replacement-text
                     end-folding? ""
                     :else original-text)]
          [(conj res (-> data-line
                       (assoc :id idx)
                       (assoc :text text)
                       (assoc :fh-status (when show-fold-head? extracted-fold-status)) ;; fh stands for folding-head
                       (assoc :arrow-position (when show-fold-head? arrow-position))
                       (assoc :toggle-event (when show-fold-head? toggle-event))
                       (assoc :fold-level (count folding-stack))
                       (assoc :fold-starter-line starter-line)))
           (inc idx)
           new-folding-stack]))
      [[] 0 '()] data-lines)))

(defn mermaid-text
  [cardume-text]
  (->> (str/split cardume-text #"\n")
    (map #(into {:original-text %}))
    (process-cardume)
    (remove #(= "open" (:fh-status %)))
    (map :text)
    (remove str/blank?)
    (str/join "\n")))
(re-frame/reg-sub
  ::mermaid-text
  :<- [::valid-cardume-text]
  mermaid-text)

(defn cardume-view
  [cardume-text]
  ;; (let [mermaid-meta (try (.-yy (.-parser (.parse mermaid cardume-text)))
  ;;                         (catch :default _
  ;;                           ""))
  ;;       actors (.getActors mermaid-meta)
  ;;       calls (.getMessages mermaid-meta)]
  ;;   (js/console.log "mermaid" mermaid-meta)
  (->> (str/split cardume-text #"\n")
    (map #(into {:original-text %}))
    (process-cardume)))
(re-frame/reg-sub
  ::cardume-view
  :<- [::cardume-text]
  cardume-view)

(defn cardume-line
  [cardume-view [_ line-num]]
  (nth cardume-view (js/parseInt line-num)))
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

(defn left-panel-size
  [app-state]
  (get-in app-state [:ui :panels :left-panel-size] "400px"))
(re-frame/reg-sub ::left-panel-size left-panel-size)

;; ---- EVENTS ----

(defn trim-lines [txt]
  (->> (str/split txt #"\n")
    (map str/trim)
    (str/join "\n")))

(defn set-cardume-text
  [app-state [_event v]]
  (try (.parse mermaid (trim-lines (mermaid-text v)))
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
        updated-lines (update lines (js/parseInt line-number) str/replace #"open|closed" new-state)]
    (set-cardume-text app-state [event (str/join "\n" updated-lines)])))
(re-frame/reg-event-db ::toggle-cardume toggle-cardume)

(defn remove-folds-without-header
  [data-lines]
  (first
    (reduce
      (fn [[res removing-level] {:keys [text original-text fold-level] folding-head? :arrow-position :as data-line}]
        (cond
          (and folding-head? (= "" text))
          [res (inc fold-level)]

          (and (= fold-level removing-level)
               (re-find #"end-fold" original-text))
          [res nil]

          :else
          [(conj res data-line) removing-level]))
      [[] nil] data-lines)))

(defn finish-line-edition
  [app-state [event]]
  (let [state-cardume-text (cardume-text app-state)
        new-cardume-text (->> (str/split state-cardume-text #"\n")
                           (map #(into {:original-text %}))
                           (process-cardume)
                           (remove-folds-without-header)
                           (map :original-text)
                           (str/join "\n"))]
    (.clearSelection ^js/dragselect @drag-select)
    (-> app-state
      (set-cardume-text [event new-cardume-text])
      (assoc-in [:ui :cardume :editing-line] nil))))
(re-frame/reg-event-db ::finish-line-edition finish-line-edition)

(defn insert-at [coll idx el]
  (let [[before after] (split-at (js/parseInt idx) coll)]
    (concat before [el] after)))

(re-frame/reg-fx
  :focus-to-element
  (fn [element-id]
    (reagent/after-render #(some-> (js/document.getElementById element-id) .focus))))

(defn get-pred
  "Returns the first element of coll that satisfy the predicate f."
  [f coll]
  (some #(when (f %) %) coll))

(defn determine-end-of-fold-group
  [cardume-text line-num]
  (->> cardume-text
    cardume-view
    (get-pred (fn [{:keys [original-text fold-starter-line]}]
                (and (re-find #"end-fold" original-text)
                     (= fold-starter-line line-num))))
    :id))

(defn lines-select-mouse-up
  [{app-state :db} [event selected-lines]]
  (let [updated-app-state (assoc-in app-state [:ui :cardume :selected-lines] selected-lines)
        num-selected (count selected-lines)
        sorted-selected (sort (map js/parseInt selected-lines))
        first-selected (first sorted-selected)
        last-selected (last sorted-selected)
        state-cardume-text (cardume-text app-state)
        lines (str/split state-cardume-text #"\n")
        end-fold-line (if (fold-header (get lines last-selected))
                        (determine-end-of-fold-group state-cardume-text last-selected)
                        (+ 2 last-selected))
        new-lines (-> lines
                    (insert-at first-selected "% closed-fold ")
                    (insert-at end-fold-line "% end-fold"))
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
        updated-lines (assoc lines (js/parseInt line-num) (str folding-header new-text))]
    (set-cardume-text app-state [event (str/join "\n" updated-lines)])))
(re-frame/reg-event-db ::set-cardume-line-text set-cardume-line-text)

(defn set-editing-line
  [{app-state :db} [_event line-num]]
  {:focus-to-element "input-line"
   :db (assoc-in app-state [:ui :cardume :editing-line] (js/parseInt line-num))})
(re-frame/reg-event-fx ::set-editing-line set-editing-line)

(defn toggle-all
  [app-state [event new-state]]
  (let [state-cardume-text (cardume-text app-state)
        lines (str/split state-cardume-text #"\n")
        updated-lines (map #(str/replace % #"open|closed" new-state) lines)]
    (set-cardume-text app-state [event (str/join "\n" updated-lines)])))
(re-frame/reg-event-db ::toggle-all toggle-all)

(defn set-panels-size
  [app-state [_event new-size]]
  (if (get-in app-state [:ui :panels :resizing-panels])
    (assoc-in app-state [:ui :panels :left-panel-size] (str new-size"px"))
    app-state))
(re-frame/reg-event-db ::set-panels-size set-panels-size)

(defn resizing-panels
  [app-state [_event new-state]]
  (assoc-in app-state [:ui :panels :resizing-panels] new-state))
(re-frame/reg-event-db ::resizing-panels resizing-panels)

;; -- Debug views --

(defn data-lines []
  [:div
   (map-indexed
     (fn [idx line]
       ^{:key (str idx line)}
       [:pre.selectable
        {:id idx
         :style {:margin "0"
                 :padding "2px 0px"}}
        line])
     (<sub [::cardume-view]))])

;; ---- Views ----

(declare initialize-dragselect)

(def code-font-family "dejavu sans mono, monospace")
(def quattrocento-font "Quattrocento, serif")
(def proza-font "Proza Libre, sans-serif")
(def roboto-mono-font "Roboto Mono, monospace")
(def code-font-size "small")
(def code-margin "0")
(def code-padding "0 10px")

(defn cardume-text-area []
  [:textarea
   {:style {:height 200 :width 400
            :margin code-margin
            :padding code-padding
            :font-family code-font-family
            :font-size code-font-size}
    :onChange #(>evt [::set-cardume-text (-> % .-target .-value)])
    :value (<sub [::cardume-text])}])

(defn cardume-line-pre-el [idx {:keys [text fold-level toggle-event arrow-position]}]
  [(with-mount-fn
     [:pre.selectable
      {:id idx
       :component-did-mount #(do
                               (some-> ^js/dragselect @drag-select .stop)
                               (reset! drag-select (initialize-dragselect)))
       :onDoubleClick #(>evt [::set-editing-line idx])
       :style {:margin code-margin
               :padding code-padding
               :font-family code-font-family
               :font-size code-font-size
               :padding-left (str (+ 10 (* 15 fold-level))"px")}}
      [:<>
       [:b {:onClick #(>evt [::toggle-cardume idx toggle-event])}
        arrow-position]
       text]])])

(defn cardume []
  (let [editing-line (js/parseInt (<sub [::editing-line]))]
    [:div#cardume
     {:style {:user-select "none"
              :width "400px"}}
     (into [:<>]
           (for [idx (range (count (<sub [::cardume-view])))]
             (let [{:keys [text original-text] :as line-data} (<sub [::cardume-line idx])]
               (if (= editing-line idx)
                 [:input#input-line.selectable
                  {:onKeyPress #(when (= (.-key %) "Enter") (>evt [::finish-line-edition]))
                   :onBlur #(>evt [::finish-line-edition])
                   :style {:font-family code-font-family
                           :font-size code-font-size}
                   :value text
                   :onChange #(>evt [::set-cardume-line-text idx original-text (-> % .-target .-value)])}]
                 [cardume-line-pre-el idx line-data]))))]))

(defn mode-comp []
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
        (<sub [::modes]))])

(defn code-comp []
  (case (<sub [::selected-mode])
    "Cardume" [cardume]
    "Cardume Text" [cardume-text-area]
    "Mermaid Text" [:pre
                    {:style {:margin code-margin
                             :padding code-padding
                             :font-family code-font-family
                             :font-size code-font-size
                             :user-select "text"}}
                    (<sub [::mermaid-text])]))

(defn diagram-comp []
  (let [mermaid-text (<sub [::mermaid-text])
        valid-diagram? (<sub [::valid-diagram?])]
    [(with-mount-fn
       [:div#sequence-1.mermaid
        {:component-did-mount #(.contentLoaded mermaid)
         :style {:opacity (if valid-diagram? "100%" "40%")}}
        mermaid-text])]))

(defn shortcut-buttons []
  [:<>
   [:input {:type "button"
            :onClick #(>evt [::toggle-all "closed"])
            :value "Collapse All"}]
   [:input {:type "button"
            :onClick #(>evt [::toggle-all "open"])
            :value "Expand All"}]])

(defn global-style []
  [:style
   "
   @import url('https://fonts.googleapis.com/css2?family=Proza+Libre:wght@400;500;600;700&family=Quattrocento&family=Roboto+Mono:wght@300;400;500;600;700&display=swap');

   .ds-selected {
     background-color: lightgray;
   }
   "])

(defn panel-splitter []
  [:div {:style {:display "flex"
                 :justify-content "center"
                 :width "6px"
                 :height "100vh"
                 :cursor "ew-resize"}
         :onMouseDown #(>evt [::resizing-panels true])}
   [:div {:style {:border-left "1px solid gray"}}]])

(defn main []
  [:<>
   [global-style]
   [:div#panel-container
    {:style {:display "flex"
             :user-select "none"}}
    [:div#left-panel
     {:style {:width (<sub [::left-panel-size])
              :min-width "20vw"}}
     [:h1 {:style {:font-family "quattrocento, serif"}}
      "Looset Cardume"]
     [shortcut-buttons]
     [diagram-comp]]
    [panel-splitter]
    [:div#right-panel
     {:style {:width (str "calc(100vw - "(<sub [::left-panel-size])")") ;; Just a testing value
              :min-width "20vw"}}
     [mode-comp]
     ;; [cardume-text-area]
     ;; [data-lines]
     [code-comp]]]])

;; ---- Initialization ----

(defn init-mousemove []
  (js/document.body.addEventListener
    "mousemove"
    #(>evt [::set-panels-size (-> % .-x)])))

(defn init-mouseup []
  (js/document.body.addEventListener
    "mouseup"
    #(>evt [::resizing-panels false])))

(defn initialize-dragselect []
  (let [ds (dragselect.
             #js {:selectables (js/document.getElementsByClassName "selectable")
                  :draggability false
                  :area (js/document.getElementById "cardume")})]
    (.subscribe ds "callback" (fn [e]
                                (re-frame/dispatch-sync [::lines-select-mouse-up (mapv (fn [i] (.-id i)) (.-items e))])
                                (.clearSelection ds)))
    ds))

(def initial-state
  {:domain {:cardume-text "sequenceDiagram\nparticipant A as Aliased A\nA->>B: a\n% open-fold A->>B: bc\nB->>A: b\nA-->>B: c\n% end-fold\nB->>B: d\nB-->A: e\nB->>A: f\nA->>A: g\nA->>B: h"}
   :ui {:panels {:resizing-panels false
                 :left-panel-size "65vw"}
        :cardume {:selected-lines {}
                  :editing-line nil}
        :mode {:selected "Cardume"
               :items [{:id "Cardume"}
                       {:id "Cardume Text"}
                       {:id "Mermaid Text"}]}}})

(defn initialize-mermaid []
  (.initialize mermaid
    #js {:theme "forest"}))

(re-frame/reg-event-db ::set-app-state
  (fn [_ [event application-state]]
    (set-cardume-text application-state [event (get-in application-state [:domain :cardume-text])])))

(defn init-state []
  (re-frame/dispatch-sync [::set-app-state initial-state]))

(defn ^:dev/after-load mount-app-element []
  (when ^boolean js/goog.DEBUG ;; Code removed in production
    (re-frame/clear-subscription-cache!))
  (when-let [el (.getElementById js/document "root")]
    (reagent.dom/render [main] el))
  (.contentLoaded mermaid)
  (reset! drag-select (initialize-dragselect)))

(defn init []
  (init-state)
  (initialize-mermaid)
  (init-mousemove)
  (init-mouseup)
  (mount-app-element))

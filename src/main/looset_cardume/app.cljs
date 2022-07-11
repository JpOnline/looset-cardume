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

(defn selected-mode
  [app-state]
  (get-in app-state [:ui :mode :selected] "cardume"))
(re-frame/reg-sub ::selected-mode selected-mode)

(defn selected-mode-style
  [selected-mode [_ component style]]
  (case [style (= selected-mode component)]
    [:filter false] "blur(1.5px) grayscale(1)"
    [:transform false] "scale(0.9)"
    nil))
(re-frame/reg-sub
  ::selected-mode-style
  :<- [::selected-mode]
  selected-mode-style)

(defn arrow-icon [{{transform :transform} :style}]
  [:svg
   {:width "2.8404655mm"
    :height "3.2798767mm"
    :viewBox "0 0 2.8404655 3.2798767"
    :style {:transform transform
            :margin "0 4px 0 0"}}
   [:g {:transform "translate(-58.753942,-132.43234)"}
    [:path
     {:d "m 270.81166,157.53724 0,-1.63995 0,-1.63994 1.42023,0.81997 1.42024,0.81997 -1.42024,0.81997 z"
      :style {:fill "#545454"}
      :transform "translate(-212.05772,-21.825018)"}]]])

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
                                              [[arrow-icon]
                                               "open"]
                                              [[arrow-icon
                                                {:style {:transform "rotate(90deg)"}}]
                                               "closed"])
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

(defn diagram-zoom
  [app-state]
  (get-in app-state [:ui :diagram :zoom] 1))
(re-frame/reg-sub ::diagram-zoom diagram-zoom)

;; ---- EVENTS ----

(defn trim-lines [txt]
  (->> (str/split txt #"\n")
    (map str/trim)
    (str/join "\n")))

(re-frame/reg-fx
  :set-url-state
  (fn [cardume-text]
    (let [loc js/window.location]
      (js/window.history.replaceState
        nil nil
        (str loc.origin loc.pathname"?diagram="
             (js/encodeURIComponent cardume-text))))))

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

(defn set-cardume-text*
  [{app-state :db} [event v]]
  (let [new-app-state (set-cardume-text app-state [event v])]
    {:db new-app-state
     :set-url-state (get-in new-app-state [:ui :validation :valid-cardume-text])}))
(re-frame/reg-event-fx ::set-cardume-text set-cardume-text*)

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

(defn mouse-moved
  [app-state [_event x y]]
  (let [[_ y-init] (get-in app-state [:ui :diagram :zoom-start-point])
        resizing-panels? (get-in app-state [:ui :panels :resizing-panels])
        zooming? (get-in app-state [:ui :diagram :zooming?])
        init-zoom (get-in app-state [:ui :diagram :init-zoom] 1)]
    (cond-> app-state
      resizing-panels?
      (assoc-in [:ui :panels :left-panel-size] (str x"px"))

      zooming?
      (assoc-in [:ui :diagram :zoom] (+ init-zoom (* 0.008 (- y y-init)))))))
(re-frame/reg-event-db ::mouse-moved mouse-moved)

(defn resizing-panels
  [app-state [_event new-state]]
  (assoc-in app-state [:ui :panels :resizing-panels] new-state))
(re-frame/reg-event-db ::resizing-panels resizing-panels)

(defn mouse-up
  [app-state]
  (-> app-state
    (resizing-panels [::mouse-up false])
    (assoc-in [:ui :diagram :zooming?] false)))
(re-frame/reg-event-db ::mouse-up mouse-up)

(defn mark-zoom-start-point
  [app-state [_event x y]]
  (-> app-state
    (assoc-in [:ui :diagram :zooming?] true)
    (assoc-in [:ui :diagram :zoom-start-point] [x y])
    (assoc-in [:ui :diagram :init-zoom] (get-in app-state [:ui :diagram :zoom]))))
(re-frame/reg-event-db ::mark-zoom-start-point mark-zoom-start-point)

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
   {:style {:flex-grow "1"
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
   {:style {:display "flex"
            :justify-content "space-evenly"
            :max-width "380px"
            :align-items "center"
            :padding "10px"}}
   [:svg#cardume-icon
    {:onClick #(>evt [::select-mode "cardume"])
     :style {:cursor "pointer"
             :filter (<sub [::selected-mode-style "cardume" :filter])
             :transform (<sub [::selected-mode-style "cardume" :transform])}
     :width "40" :height "40" :viewBox "0 0 40 40"}
    [:circle {:id "circle" :style {:fill "#fff"} :cx "19.438459" :cy "19.438021" :r "19.438021"}]
    [:path {:id "L" :d "m 13.619455,9.7731986 q -0.08054,-1.12756 -0.36243,-1.57053 -0.28189,-0.44297 -0.60405,-0.72486 -0.36243,-0.28189 -0.92621,-0.32216 -0.56378,-0.08054 -1.2081,-0.08054 l -0.20135,-0.16108 -0.12081,-0.72486 0.08054,-0.16108 q 0.28189,0 0.92621,0.04027 0.64432,0.04027 1.32891,0.08054 0.68459,0.04027 1.32891,0.08054 0.64432,0.04027 0.92621,0.04027 h 0.76513 q 0.28189,0 0.92621,-0.04027 0.64432,-0.04027 1.32891,-0.08054 0.68459,-0.04027 1.32891,-0.08054 0.644321,-0.04027 0.926211,-0.04027 l 0.08054,0.16108 -0.12081,0.72486 -0.16108,0.16108 q -0.644321,0 -1.248371,0.08054 -0.56378,0.04027 -0.92621,0.32216 -0.32216,0.28189 -0.64432,0.76513 -0.28189,0.44297 -0.36243,1.53026 -0.04027,0.5637804 -0.08054,1.0470204 0,0.48324 0,1.12756 0,0.64432 0,1.57053 0,0.92621 0,2.37593 v 7.40968 q 0,1.40945 0,2.33566 0,0.92621 0,1.6108 0.04027,0.68459 0.08054,1.28864 0.04027,0.60405 0.08054,1.36918 0.04027,0.76513 0.84567,0.76513 h 3.422951 q 2.4162,0 3.6243,-0.08054 1.24837,-0.12081 1.77188,-0.44297 0.56378,-0.32216 0.68459,-0.96648 0.12081,-0.68459 0.4027,-1.77188 l 0.12081,-0.16108 h 1.04702 l -0.72486,5.11429 -0.32216,0.4027 q -0.20135,0 -0.76513,-0.04027 -0.52351,0 -1.24837,-0.04027 -0.68459,-0.04027 -1.44972,-0.08054 -0.72486,0 -1.24837,0 h -8.134541 q -0.28189,0 -0.92621,0.04027 -0.64432,0.04027 -1.32891,0.08054 -0.68459,0.04027 -1.32891,0.08054 -0.64432,0.04027 -0.92621,0.04027 l -0.08054,-0.16108 0.12081,-0.72486 0.20135,-0.16108 q 0.64432,0 1.2081,-0.04027 0.56378,-0.08054 0.92621,-0.36243 0.32216,-0.28189 0.60405,-0.8054 0.28189,-0.52351 0.4027,-1.48999 0.04027,-0.28189 0.04027,-0.64432 0.04027,-0.4027 0.04027,-0.96648 0,-0.56378 0,-1.36918 0,-0.8054 0,-2.0135 v -8.17481 q 0,-1.40945 0,-2.37593 0,-0.96648 -0.04027,-1.65107 0,-0.68459 -0.04027,-1.16783 0,-0.48324 -0.04027,-0.9664804 z"}]
    [:path {:id "left" :style {:fill "#44674b"} :d "M 5.7929688,5.6646805 A 19.438021,19.438021 0 0 0 0,19.438118 19.438021,19.438021 0 0 0 5.7929688,33.211556 Z"}]
    [:path {:id "right" :style {:fill "#44674b"} :d "M 31.765625,4.4088211 V 34.428353 A 19.438021,19.438021 0 0 0 38.876953,19.438118 19.438021,19.438021 0 0 0 31.765625,4.4088211 Z"}]]
   [:svg#text-icon
    {:onClick #(>evt [::select-mode "text"])
     :width "54" :height "36" :viewBox "0 0 24 16"
     :style {:cursor "pointer"
             :padding "0 5px"
             :filter (<sub [::selected-mode-style "text" :filter])
             :transform (<sub [::selected-mode-style "text" :transform])}}
    [:path {:style {:fill "none" :stroke "#000" :stroke-width "0.264582px" :stop-color "#000000"}
            :d "m 1.371658,10.570294 h 19.849917 z m 0,0 h 19.849917 z m -2e-6,2.200418 H 18.08878 Z M 8.965422,8.369871 h 12.256152 z m 2.19382,-6.60126 H 21.221574 Z M 8.965418,4.010492 H 21.22157 Z m 4e-6,2.158964 H 21.221574 Z M 6.1141241,9.215371 q -0.07808,0 -0.245404,-0.01116 -0.16732,-0.01115 -0.35695,-0.02231 -0.18963,-0.01116 -0.356951,-0.02231 -0.16732,-0.01115 -0.245403,-0.01115 H 4.273598 q -0.07808,0 -0.245403,0.01115 -0.167321,0.01115 -0.356951,0.02231 -0.178475,0.01116 -0.345795,0.02231 -0.167321,0.01116 -0.245404,0.01116 l -0.02231,-0.04462 0.03346,-0.200784 0.05577,-0.04462 q 0.178476,0 0.40157,-0.01116 0.223094,-0.02231 0.312331,-0.100392 0.08924,-0.07808 0.18963,-0.200785 0.100392,-0.122701 0.122702,-0.435033 0.01115,-0.07808 0.01115,-0.178475 0.01116,-0.111547 0.01116,-0.267713 0.01115,-0.156166 0.01115,-0.37926 0,-0.223094 0,-0.557735 V 4.53039 q 0,-0.725056 -0.01115,-1.282791 Q 4.184348,2.67871 4.173198,2.388688 H 4.050496 q -0.557735,0 -0.914685,0.01115 -0.356951,0 -0.580045,0.02231 Q 2.343827,2.433298 2.23228,2.455608 2.131888,2.477918 2.087269,2.511378 1.841865,2.645235 1.786092,3.091423 l -0.04462,0.04462 H 1.484914 l 0.07808,-1.271635 0.04462,-0.100393 0.06693,-0.03346 q 0.08924,0.03346 0.390414,0.06693 0.301177,0.02231 0.658128,0.03346 0.35695,0.01115 0.680436,0.02231 0.334641,0 0.479653,0 h 1.4054921 q 0.145011,0 0.468497,0 0.334641,-0.01116 0.6915919,-0.02231 0.35695,-0.01116 0.658127,-0.03346 0.301177,-0.03346 0.390414,-0.06693 l 0.06693,0.03346 0.04462,0.100393 0.08924,1.271635 H 7.430374 L 7.385754,3.091423 Q 7.329984,2.645235 7.084578,2.511378 7.039958,2.477918 6.928412,2.455608 6.828019,2.433298 6.604925,2.422148 6.3929861,2.399838 6.0360361,2.399838 5.6790851,2.388688 5.1325051,2.388688 h -0.122702 q -0.01115,0.323486 -0.02231,0.847757 0,0.524271 0,1.28279 V 6.5717 q 0,0.401569 0,0.658127 0,0.245404 0,0.412724 0.01115,0.167321 0.01115,0.290023 0.01115,0.111547 0.02231,0.245403 0.02231,0.312332 0.122702,0.435033 0.100392,0.122702 0.18963,0.200785 0.08924,0.07808 0.312331,0.100392 0.223094,0.01116 0.401569,0.01116 l 0.05577,0.04462 0.03346,0.200784 z M 0.19566501,0.18565901 H 22.612891 V 14.645259 H 0.19566501 Z"}]]
   [:svg#mermaid-icon
    {:onClick #(>evt [::select-mode "mermaid"])
     :style {:cursor "pointer"
             :filter (<sub [::selected-mode-style "mermaid" :filter])
             :transform (<sub [::selected-mode-style "mermaid" :transform])}
     :width "40" :height "40" :viewBox "0 0 80 80"}
    [:defs
     [:linearGradient {:id "linearGradient83282"}
      [:stop {:style {:stop-color "#5b6cdb" :stop-opacity "1"} :offset "0" :id "stop83424"}]
      [:stop {:style {:stop-color "#864bcb" :stop-opacity "1"} :offset "1" :id "stop83426"}]]
     [:linearGradient {:href "#linearGradient83282" :id "linearGradient83914" :x1 "11.463596" :y1 "3.8692131" :x2 "103.4958" :y2 "3.3014159" :gradientUnits "userSpaceOnUse" :gradientTransform "translate(-28.239843,35.838281)"}]]
    [:path {:id "path63575" :style {:fill "url(#linearGradient83914)" :stroke "#000" :stroke-width "0.2"}
            :d "M 38.516015,0.1 C 17.299425,0.099975 0.099975,17.299426 0.1,38.516015 0.14021,52.650792 7.939263,65.62171 20.404687,72.285547 27.352299,68.095451 38.42437,58.236238 36.150781,38.668359 c 0.03006,-0.324814 -0.190537,-0.193896 -0.417969,-0.216797 0,0 -17.141604,-0.600556 -25.964844,-24.195312 0,0 3.069667,1.539888 4.671876,1.863281 4.731966,0.955108 10.433623,-1.137145 14.480468,0.263672 0,0 2.921463,0.720837 6.824219,5.316406 0,0 0.612641,-7.159933 5.068359,-9.816406 4.455719,-2.6564734 8.863694,-1.47406 12.1875,-8.484375 0,0 9.627799,15.374042 -8.927734,33.582031 -0.105395,-0.03178 10.295363,20.207571 -4.990234,39.910156 20.979109,-0.309313 37.829519,-17.39362 37.849619,-38.375 C 76.932071,17.299423 59.732607,0.099972 38.516015,0.1 Z"}]]])

(defn code-comp []
  [:div
   {:style {:overflow "auto"
            :display "flex"
            :flex-grow "1"
            :padding "7px 0"}}
   (case (<sub [::selected-mode])
     "cardume" [cardume]
     "text"    [cardume-text-area]
     "mermaid" [:pre
                {:style {:margin code-margin
                         :padding code-padding
                         :font-family code-font-family
                         :font-size code-font-size
                         :user-select "text"}}
                (<sub [::mermaid-text])])])

(defn diagram-comp []
  (let [mermaid-text (<sub [::mermaid-text])
        valid-diagram? (<sub [::valid-diagram?])]
    [(with-mount-fn
       [:div#sequence-1.mermaid
        {:component-did-mount #(.contentLoaded mermaid)
         :onMouseDown #(>evt [::mark-zoom-start-point (-> % .-clientX) (-> % .-clientY)])
         :style {:opacity (if valid-diagram? "100%" "40%")
                 :overflow "auto"
                 :flex-grow "1"}}
        mermaid-text])]))

(defn shortcut-buttons []
  [:div#shortcut-buttons
   {:style {:display "flex"
            :justify-content "space-evenly"
            :padding "10px"}}
   [:button.button-1
    {:title "collapse all"
     :onClick #(>evt [::toggle-all "closed"])}
    [:svg
     {:width "30" :height "30" :fill "currentColor" :viewBox "0 0 16 16" :xmlns "http://www.w3.org/2000/svg"}
     [:path {:fill-rule "evenodd" :d "M1 8a.5.5 0 0 1 .5-.5h13a.5.5 0 0 1 0 1h-13A.5.5 0 0 1 1 8zm7-8a.5.5 0 0 1 .5.5v3.793l1.146-1.147a.5.5 0 0 1 .708.708l-2 2a.5.5 0 0 1-.708 0l-2-2a.5.5 0 1 1 .708-.708L7.5 4.293V.5A.5.5 0 0 1 8 0zm-.5 11.707-1.146 1.147a.5.5 0 0 1-.708-.708l2-2a.5.5 0 0 1 .708 0l2 2a.5.5 0 0 1-.708.708L8.5 11.707V15.5a.5.5 0 0 1-1 0v-3.793z"}]]]
   [:button.button-1
    {:title "expand all"
     :onClick #(>evt [::toggle-all "open"])}
    [:svg
     {:width "30" :height "30" :fill "currentColor" :viewBox "0 0 16 16" :xmlns "http://www.w3.org/2000/svg"}
     [:path {:fill-rule "evenodd" :d "M1 8a.5.5 0 0 1 .5-.5h13a.5.5 0 0 1 0 1h-13A.5.5 0 0 1 1 8zM7.646.146a.5.5 0 0 1 .708 0l2 2a.5.5 0 0 1-.708.708L8.5 1.707V5.5a.5.5 0 0 1-1 0V1.707L6.354 2.854a.5.5 0 1 1-.708-.708l2-2zM8 10a.5.5 0 0 1 .5.5v3.793l1.146-1.147a.5.5 0 0 1 .708.708l-2 2a.5.5 0 0 1-.708 0l-2-2a.5.5 0 0 1 .708-.708L7.5 14.293V10.5A.5.5 0 0 1 8 10z"}]]]])

(defn global-style []
  (let [zoom (<sub [::diagram-zoom])]
    [:style
     (str "
     @import url('https://fonts.googleapis.com/css2?family=Proza+Libre:wght@400;500;600;700&family=Quattrocento&family=Roboto+Mono:wght@300;400;500;600;700&display=swap');

      *::-webkit-scrollbar-track {
          background: rgb(51 51 51 / 10%);
      }
      *::-webkit-scrollbar-thumb {
          background: #bbb;
      }
      *::-webkit-scrollbar {
          width: 5px;
          height: 5px;
      }

     .ds-selected {
       background-color: lightgray;
     }

     #sequence-1 svg {
       transform: scale("zoom");
       transform-origin: top left;
       cursor: crosshair;
     }

     .button-1 {
       display: flex;
       background-color: #4c4c4c;
       border-radius: 8px;
       border-style: none;
       box-sizing: border-box;
       color: #FFFFFF;
       cursor: pointer;
       font-family: "quattrocento-font";
       font-size: 14px;
       font-weight: 500;
       line-height: 20px;
       list-style: none;
       margin: 0;
       outline: none;
       padding: 10px;
       position: relative;
       text-align: center;
       text-decoration: none;
       transition: color 100ms;
       vertical-align: baseline;
       user-select: none;
       -webkit-user-select: none;
       touch-action: manipulation;
     }

     .button-1:hover,
     .button-1:focus {
       background-color: #7c7c7c;
     }
     ")]))

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
             :user-select "none"
             :max-height "100vh"}}
    [:div#left-panel
     {:style {:width (<sub [::left-panel-size])
              :min-width "20vw"
              :display "flex"
              :flex-direction "column"}}
     [:div {:style {:font-family quattrocento-font
                    :font-size "2em"
                    :padding "10px"
                    :border-bottom "1px solid gray"}}
      "Looset Cardume"]
     [diagram-comp]
     [shortcut-buttons]]
    [panel-splitter]
    [:div#right-panel
     {:style {:width (str "calc(100vw - "(<sub [::left-panel-size])")") ;; Just a testing value
              :display "flex"
              :flex-direction "column"
              :min-width "20vw"}}
     [mode-comp]
     ;; [cardume-text-area]
     ;; [data-lines]
     [code-comp]]]])

;; ---- Initialization ----

(defn init-mousemove []
  (js/document.body.addEventListener
    "mousemove"
    #(>evt [::mouse-moved (-> % .-x) (-> % .-y)])))

(defn init-mouseup []
  (js/document.body.addEventListener
    "mouseup"
    #(>evt [::mouse-up false])))

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
        :mode {:selected "cardume"}
        :diagram {:zoom 1
                  :zooming? false}}})

(defn initialize-mermaid []
  (.initialize mermaid
    #js {:theme "default"}))

(re-frame/reg-event-db ::set-app-state
  (fn [_ [event application-state]]
    (if-let [diagram (.get (js/URLSearchParams. js/window.location.search) "diagram")]
      (set-cardume-text application-state [event diagram])
      (set-cardume-text application-state [event (get-in application-state [:domain :cardume-text])]))))

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

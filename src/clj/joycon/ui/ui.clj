(ns joycon.ui.ui
  (:require
    [clojure.string :as string]
    [joycon.ui.core :as jc]
    [joycon.core :as j]
    [uncomplicate.neanderthal.core :as nc]
    [clojure.core.async :as async :refer [chan <!! >!! <! >! put! close! pipe go go-loop dropping-buffer]]
    [cljfx.api :as fx]
    [uncomplicate.neanderthal.native :as nn])
  (:import
    [uncomplicate.neanderthal.internal.host.buffer_block RealBlockVector IntegerBlockVector]))

(defmulti component
  (fn [{:keys [kind value]}]
    (or kind (type value))))

(defmethod component :default
  [{:keys [path value] :as s}]
  {:fx/type :label
   :text (str path " : " value)})

(defn text-component [{:keys [path value]}]
  {:fx/type :text-field
   :h-box/margin 8
   :on-text-changed {:event/type :change-text :path path}
   :text (str value)})

(defmethod component clojure.lang.Symbol [{:keys [path value] :as s}]
  (text-component s))

(defn box-component [{:keys [depth path value]}]
  ((fn [c] (if (= path [:exp]) c (assoc c :h-box/margin 8)))
   {:fx/type :h-box
    :alignment :center
    :style {:-fx-border-style :solid
            :-fx-border-radius 8
            :-fx-background-color
            (str "rgb(" (string/join "," (repeat 3 (* 30 depth))) ")")}
    :children
    (map-indexed
      (fn [i c]
        (component {:path (conj path i) :value c
                    :depth (mod (inc depth) 3)}))
      value)}))

(defmethod component clojure.lang.PersistentVector [{:keys [depth path value] :as s}]
  (box-component s))

(defmethod component clojure.lang.PersistentList [{:keys [depth path value] :as s}]
  (box-component s))

(defmethod component clojure.lang.PersistentArrayMap [{:keys [depth path value] :as s}]
  (box-component s))

(defmethod component RealBlockVector
  [{:keys [path value] :as s}]
  {:fx/type :label
   :text (string/join " "
           (map (fn [i] (let [x (nc/entry value i)] (str (if (< x 0) "" "+") (format "%5.5e" x)))) (range 3)))})

(defmethod component :line-chart-v3
   [{:keys [path value] :as s}]
  {:fx/type :line-chart
   :x-axis {:fx/type :number-axis
            :auto-ranging true}
   :y-axis {:fx/type :number-axis
            :auto-ranging true}
   :data (map-indexed
           (fn [j c] {:fx/type :xy-chart-series
                      :name (str "acceleration-" (name c))
                      :data (map-indexed (fn [i v] {:fx/type :xy-chart-data :x-value i :y-value (nc/entry v j)}) value)})
           [:x :y :z])})

(defmethod component :line-chart
   [{:keys [path value title] :as s}]
  {:fx/type :line-chart
   :create-symbols false
   :vertical-grid-lines-visible false
   :horizontal-grid-lines-visible false
   :x-axis {:fx/type :number-axis
            :lower-bound 0
            :upper-bound 256
            :auto-ranging false}
   :y-axis {:fx/type :number-axis
            :lower-bound -0.001
            :upper-bound +0.001
            :auto-ranging true}
   :data [{:fx/type :xy-chart-series
           :name title
           :data (map-indexed (fn [i v] {:fx/type :xy-chart-data :x-value i :y-value v}) value)}]})

(defmethod component :bar-chart
   [{:keys [path value title] :as s}]
  {:fx/type :bar-chart
   :legend-visible false
   :x-axis {:fx/type :category-axis}
   :y-axis {:fx/type :number-axis
            :auto-ranging true}
   :data [{:fx/type :xy-chart-series
           :name title
           :data (map-indexed (fn [i v] {:fx/type :xy-chart-data :x-value (str i) :y-value v}) value)}]})

(defn root
  ([{v :imu :as state}]
   {:fx/type :stage
    :showing true
    :title "imu"
    :width 1024
    :height 1024
    :scene
    {:fx/type :scene
     :root
     {:fx/type :v-box
      :alignment :center
      :children
      [
       {:fx/type :button
        :text "stop"
        :on-action {:event/type :click/stop}}
       (component {:value (:a (last v))})
       (component {:value (:r (last v))})
       {:fx/type :h-box
        :children
         [{
          :fx/type :v-box
          :children
          [(component {:kind :line-chart :title "ax" :value (map (fn [x] (nc/entry (:a x) 0)) v)})
           (component {:kind :line-chart :title "ay" :value (map (fn [x] (nc/entry (:a x) 1)) v)})
           (component {:kind :line-chart :title "az" :value (map (fn [x] (nc/entry (:a x) 2)) v)})]}

         {:fx/type :v-box
          :children
          [(component {:kind :line-chart :title "gx" :value (map (fn [x] (nc/entry (:r x) 0)) v)})
           (component {:kind :line-chart :title "gy" :value (map (fn [x] (nc/entry (:r x) 1)) v)})
           (component {:kind :line-chart :title "gz" :value (map (fn [x] (nc/entry (:r x) 2)) v)})]}]}
       ]}}}))

(defn start! [state]
  (fx/mount-renderer state
    (fx/create-renderer
      :middleware (fx/wrap-map-desc assoc :fx/type root)
      :opts {:fx.opt/map-event-handler jc/handle-event})))

(defn push [n v i] (if (>= (count v) n) (conj (subvec v 1 n) i) (conj v i)))

(defn view-joycon! [{dc :data-channel mc :message-channel ic :input-channel :as joycon}]
  (let [state (atom {:imu (vec (repeat 256 (j/->imu-data (nn/dv 0 0 0) (nn/dv 0 0 0)))) :joycon joycon})
        renderer (fx/create-renderer :middleware (fx/wrap-map-desc assoc :fx/type root)
          :opts {:fx.opt/map-event-handler (jc/handle-event state)})
        data->state (go-loop []
                       (let [d (<! dc)]
                         (if d
                           (do
                            (swap! state update :imu (fn [v] (push 256 v d)))
                            (recur))
                           :done)))
        check-msgs (go-loop []
                      (let [m (<! mc)]
                        (if (= m :stop)
                          (do (println "stop!") (when dc (close! dc)) (when ic (close! ic)) :done)
                          (recur))))
       ]
    (swap! state (fn [s]
      (-> s (assoc-in [:joycon :data-update-channel] data->state) (assoc-in [:joycon :check-messages-channel] check-msgs)
        (assoc :renderer renderer))))
    (fx/mount-renderer state renderer)
    state))

(defn debug-joycon! [joycon]
  (-> joycon
    (j/add-imu-data-channel)
    (j/set-output-report-preset! :set-input-report-mode/full)
    (view-joycon!)))

(defn undebug-joycon! [state-atom]
  (put! (:message-channel (:joycon @state-atom)) :stop))
(ns joycon.ui.core
  (:require
    [clojure.core.async :as async :refer [chan <!! >!! <! >! put! close! pipe go go-loop dropping-buffer]]))

(def state (atom {:imu {}}))

(defmulti handle (fn [state event] (:event/type event)))

(defn handle-event [event]
  (swap! state (fn [s] (handle s event))))

(defmethod handle :change-text [state {path :path value :fx/event}]
  (assoc-in state path (str value)))

(defmethod handle :sample/imu [state {path :path value :value}]
  (assoc-in state path value))

(defmethod handle :click/stop [{{mc :message-channel} :joycon :as state} {path :path value :value}]
  (when mc (put! mc :stop))
  state)
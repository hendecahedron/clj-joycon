(ns joycon.core
  "

    joycon library


    References:


    https://github.com/dekuNukem/Nintendo_Switch_Reverse_Engineering

    https://github.com/elgoupil/joyconLib

    https://github.com/Looking-Glass/JoyconLib

  "
  (:require
    [joycon.stants :as joy]
    [clojure.string :as string])
  (:import
    [purejavahidapi PureJavaHidApi]
    [purejavahidapi HidDevice]
    [purejavahidapi HidDeviceInfo]
    [purejavahidapi DeviceRemovalListener]
    [purejavahidapi InputReportListener]
    [purejavahidapi PureJavaHidApi]))

(set! *warn-on-reflection* true)

(def zero64 (vec (repeat 64 0)))
(def zero54 (vec (repeat 54 0)))
(def zero48 (vec (repeat 48 0)))
(def zero16 (vec (repeat 16 0)))
(def zero08 (vec (repeat  8 0)))

(defn bytes64 [bytes-map]
  (reduce
    (fn [b [index value]]
      (aset-byte b index (byte value))
      b)
    (byte-array zero64) bytes-map))

(defn bytes-pad-right [b n]
  (into b (repeat (- n (count b)) 0)))

; https://github.com/dekuNukem/Nintendo_Switch_Reverse_Engineering/blob/master/bluetooth_hid_subcommands_notes.md#subcommands(def subcommands
(def subcommands
  {
   :set-input-report-mode/simple-hid (bytes64 {0 0x01 10 0x03 11 0x3F})
   :set-player-lights/flash          (bytes64 {0 0x01 10 0x30 11 -16})
   :set-player-lights/on             (bytes64 {0 0x01 10 0x30 11 2r00001111})
   :enable-vibration                 (bytes64 {0 0x01 10 0x48 11 0x01})
   :disable-vibration                (bytes64 {0 0x01 10 0x48 11 0x00})
   :send-vibration-lr                (bytes64 {0 0x01 2 -62 3 -56 4 0x03 5 0x72 6 -62 7 -56 8 0x03 9 0x72})
   })

(defn devices []
  (map
    (fn [^HidDeviceInfo d]
      {
        :device     d
        :vendor-id  (.getVendorId d)
        :product-id (.getProductId d)
      })
    (PureJavaHidApi/enumerateDevices)))

(defn ^HidDeviceInfo device-info [m]
  (if-let [d (first (filter #(= m (dissoc % :device)) (devices)))]
    (:device d)
    nil))

(defn ^HidDevice open-device [^HidDeviceInfo d]
  (PureJavaHidApi/openDevice d))

(defn close-device [{^HidDevice d :device}]
  (.close d))

(defn joycon [side]
  (if-let [di (device-info {:vendor-id  joy/vendor-id
                            :product-id ({:left joy/joycon-left :right joy/joycon-right} side)})]
    {:device (open-device di) :device-info di}
    nil))

(defn with-set-output-report [{^HidDevice joycon :device :as j} data]
  (assoc j :result (.setOutputReport joycon (byte 0x01) (byte-array data) 64) :data data))

(defn set-output-report [{^HidDevice joycon :device :as j} subcommand-key]
  (.setOutputReport joycon (byte 0x01) (subcommands subcommand-key) 64)
  j)

(defn debug [{^HidDevice joycon :device :as j}]
  (.setInputReportListener joycon
    (reify InputReportListener
     (onInputReport [this source reportID data reportLength]
      (println ">" (string/join " " data))
      (when (== 1 (aget data 2))
        (close-device j))))))
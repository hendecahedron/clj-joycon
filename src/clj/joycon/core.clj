(ns joycon.core
  "

  Clojure joycon functions

  (def jl (joycon :left)

  returns a joycon (vibration & IMU enabled by default)

  (close-device jl)

  to close it - if you try to open it again the JVM will probably crash


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

(def global-packet-number (atom (byte 0)))

(def zero64 (vec (repeat 64 0)))
(def zero54 (vec (repeat 54 0)))
(def zero48 (vec (repeat 48 0)))
(def zero16 (vec (repeat 16 0)))
(def zer08  (vec (repeat  8 0)))
(def zer06  (vec (repeat  6 0)))

(defn bytez
  ([bytes-map]
    (bytez zero64 bytes-map))
  ([zeros bytes-map]
    (reduce
     (fn [b [index value]]
       (aset-byte b index (byte value))
       b)
     (byte-array zeros) bytes-map)))

(defn bytes-pad-right [b n]
  (into b (repeat (- n (count b)) 0)))

; https://github.com/dekuNukem/Nintendo_Switch_Reverse_Engineering/blob/master/bluetooth_hid_subcommands_notes.md#subcommands(def subcommands
(def subcommands
  {
   :set-input-report-mode/simple-hid (bytez zero16 {0 0x01 10 0x03 11 0x3F})
   :set-input-report-mode/standard   (bytez zero16 {0 0x01 10 0x03 11 0x30})
   :set-player-lights/flash          (bytez zero16 {0 0x01 10 0x30 11 -16})
   :set-player-lights/on             (bytez zero16 {0 0x01 10 0x30 11 2r00001111})
   :set-player-lights/off            (bytez zero16 {0 0x01 10 0x30 11 2r00000000})
   :enable-vibration                 (bytez zero16 {0 0x01 10 0x48 11 0x01})
   :disable-vibration                (bytez zero16 {0 0x01 10 0x48 11 0x00})
   :enable-imu                       (bytez zero16 {0 0x01 10 0x40 11 0x01})
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

(defn with-packet-number [bytes]
  (swap! global-packet-number (fn [x] (byte (mod (inc x) 15))))
  (aset-byte bytes 1 @global-packet-number)
  bytes)

(defn with-set-output-report [{^HidDevice joycon :device :as j} data]
  (assoc j :result (.setOutputReport joycon (byte 0x01) (with-packet-number (byte-array data)) (count data)) :data data))

(defn set-output-report [{^HidDevice joycon :device :as j} subcommand-key]
  (.setOutputReport joycon (byte 0x01) (with-packet-number (subcommands subcommand-key)) 16)
  j)

(defn joycon [side]
  (if-let [di (device-info {:vendor-id  joy/vendor-id
                            :product-id ({:left joy/joycon-left :right joy/joycon-right} side)})]
    (-> {:device (open-device di) :device-info di :side side}
      (set-output-report :enable-imu)
      (set-output-report :enable-vibration))
    nil))

(defn decode-stick-data
  ([side ^bytes data]
    (let [i (if (= :left side) 6 9)
          b0 (aget data i) b1 (aget data (+ i 1)) b2 (aget data (+ i 2))]
      [(bit-or b0 (bit-shift-left (bit-and b1 0xf) 0x08))
       (bit-or (bit-shift-right b1 4) (bit-shift-left b2 4))])))

(defn ^InputReportListener ignore-reports []
  (reify InputReportListener
    (onInputReport [this source reportID data reportLength])))

(defn debug-stick-data [{^HidDevice joycon :device side :side :as j}]
  (.setInputReportListener joycon
    (reify InputReportListener
     (onInputReport [this source reportID data reportLength]
       (println ">" (decode-stick-data side data))
       (when (== 1 (bit-and (aget data 4) 0x01))
         (.setInputReportListener joycon (ignore-reports)))))))

(defn debug [{^HidDevice joycon :device :as j}]
  (.setInputReportListener joycon
    (reify InputReportListener
     (onInputReport [this source reportID data reportLength]
      (println ">" (string/join " " (map #(format "%02x" %) data)))
      (when (== 1 (bit-and (aget data 4) 0x01))
        (.setInputReportListener joycon (ignore-reports)))))))
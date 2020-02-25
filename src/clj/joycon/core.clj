(ns joycon.core
  "

  Joycon functions


  (def jl (joycon! :left)

  returns a joycon (vibration & IMU enabled by default)

  (send-vibrations! jl [[150 160 0.79] [200 80 0.0]])

  (close-joycon! jl)

  to close it


  "
  (:require
    [clojure.core.async :as async :refer [chan <!! >!! <! >! put! close! pipe go go-loop dropping-buffer]]
    [joycon.constants :as joy]
    [uncomplicate.neanderthal.core :as nc]
    [uncomplicate.neanderthal.native :as nn]
    [uncomplicate.neanderthal.vect-math :as nvm]
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

; bit operations - 'b' looks like a 1 and a 0 superimposed
(def b< bit-shift-left)
(def b> bit-shift-right)
(def b& bit-and)
(def b| bit-or)

; various vectors of zeros
(def zero64 (vec (repeat 64 0)))
(def zero54 (vec (repeat 54 0)))
(def zero48 (vec (repeat 48 0)))
(def zero16 (vec (repeat 16 0)))
(def zer08  (vec (repeat  8 0)))
(def zer06  (vec (repeat  6 0)))

(defn bytez
  "
    Returns a byte array of zeros of the given size
    with the given map of indices to bytes set

    e.g. (bytez zero16 {0 0x01 10 0x48 11 0x01})
    returns a 16 byte array with all zeros except
    bytes 0 set to 0x01, byte 10 set to 0x48 and byte 11 set to 0x01
  "
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

; https://github.com/dekuNukem/Nintendo_Switch_Reverse_Engineering/blob/master/bluetooth_hid_subcommands_notes.md#subcommands
(def subcommand-presets
  {
   :set-input-report-mode/simple     (bytez zero16 {0 0x01 10 0x03 11 0x3F})
   :set-input-report-mode/full       (bytez zero16 {0 0x01 10 0x03 11 0x30})
   :set-input-report-mode/nfc-ir     (bytez zero16 {0 0x01 10 0x03 11 0x31})
   :set-input-report-mode/mcu-state  (bytez zero16 {0 0x01 10 0x03 11 0x23})
   :set-player-lights/flash          (bytez zero16 {0 0x01 10 0x30 11 -16})
   :set-player-lights/on             (bytez zero16 {0 0x01 10 0x30 11 2r00001111})
   :set-player-lights/off            (bytez zero16 {0 0x01 10 0x30 11 2r00000000})
   :enable-vibration                 (bytez zero16 {0 0x01 10 0x48 11 0x01})
   :disable-vibration                (bytez zero16 {0 0x01 10 0x48 11 0x00})
   :enable-imu                       (bytez zero16 {0 0x01 10 0x40 11 0x01})
   :spi-flash-read-factory-config    (bytez zero16 {0 0x01 10 0x10 11 0x0 12 0x60  15 0x1d})
   :spi-flash-read-user-config       (bytez zero16 {0 0x01 10 0x10 11 0x0 12 -0x80 15 0x1d})
   })

(defn print-hex [x] (format "%02x" x))

(defn print-hex-map [m]
  (into {} (map (juxt key (comp (fn [b] (string/join " " (map print-hex b))) val)) m)))

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

(defn close-joycon!
  "
   Close the given joycon
  "
  [{^HidDevice d :device ic :input-report-channel dc :data-channel rc :result-channel mc :message-channel}]
  (doseq [c [ic dc rc mc]] (when c (close! c)))
  (.close d))

(defn with-packet-number [bytes]
  (swap! global-packet-number (fn [x] (byte (unchecked-remainder-int (unchecked-inc-int x) 15))))
  (aset-byte bytes 1 @global-packet-number)
  bytes)

(defn set-output-report!
  "
   Send the given joycon the given data
   (a collection of bytes)
  "
  [{^HidDevice joycon :device :as j} data]
  (.setOutputReport joycon (byte 0x01) (with-packet-number (byte-array data)) (count data)))

(defn set-output-report-preset!
  "
   Send to given joycon the given preset
   subcommand data
  "
  [{^HidDevice joycon :device :as j} subcommand-key]
  (.setOutputReport joycon (byte 0x01) (with-packet-number (subcommand-presets subcommand-key)) 16)
  j)

(defrecord Report [report-id data length])

(defn add-input-channel [{^HidDevice joycon :device :as j}]
  (let [c (chan (dropping-buffer 16))]
    (.setInputReportListener joycon
     (reify InputReportListener
       (onInputReport [this source reportID data reportLength]
         (put! c (->Report reportID data reportLength)))))
    (.setDeviceRemovalListener joycon
      (reify DeviceRemovalListener
        (onDeviceRemoval [this source]
          (close! c))))
     (assoc j :input-report-channel c)))


; !! ch must close
(defn add-velocity-channel [{:keys [data-channel] :as j}]
  (assoc j :data-channel'
    (async/transduce
      (partition-all 2)
      (fn [r [{s0 :stick} {s1 :stick}]] {:stick (mapv - s0 s1)})
      {} data-channel)))

(defn setup-joycon! [{ic :input-report-channel :as j}]
  (set-output-report-preset! j :set-input-report-mode/simple)
  (<!! ic)
  (set-output-report-preset! j :enable-imu)
  (<!! ic)
  (set-output-report-preset! j :enable-vibration)
  (<!! ic)
  j)

(defn joycon! [side]
  (if-let [di (device-info {:vendor-id  joy/vendor-id
                            :product-id ({:left joy/joycon-left :right joy/joycon-right} side)})]
    (->
      {
       :device (open-device di)
       :device-info di
       :side side
       :stop-button (if (= :left side) joy/minus-on joy/plus-on)
       :stop-button-index (if (= :left side) 4 2)
       :data-channel (chan)
       :stick-offsets (if (= :left side) [6 7 8] [9 10 11])
       }
      (add-input-channel)
      ;(add-velocity-channel)
      setup-joycon!)
    nil))

(defn decode-stick-data
  ([[i j k] ^bytes data]
    (let [b0 (aget data i)
          b1 (aget data j)
          b2 (aget data k)]
      [(b| b0 (b< (b& b1 0xf) 0x08))
       (b| (b> b1 4) (b< b2 4))])))

; https://github.com/dekuNukem/Nintendo_Switch_Reverse_Engineering/blob/master/spi_flash_notes.md#analog-stick-factory-and-user-calibration
(defn decode-3x3 [side [d0 d1 d2 d3 d4 d5 d6 d7 d8]]
  (let [
         c0 (b| (b& (b< d1 8) 0xf00) d0)
         c1 (b| (b< d2 4) (b> d1 4))
         c2 (b| (b& (b< d4 8) 0xf00) d3)
         c3 (b| (b< d5 4) (b> d4 4))
         c4 (b| (b& (b< d7 8) 0xf00) d6)
         c5 (b| (b< d8 4) (b> d7 4))
         [cx cy xa ya xb yb] (if (= :left side) [c2 c3 c0 c1 c4 c5] [c0 c1 c4 c5 c2 c3])
       ]
       {
         :cx cx
         :cy cy
         :minx (- cx xb)
         :maxx (+ cx xa)
         :miny (- cy yb)
         :maxy (+ cy ya)
       }))

(defn decode-stick-params [params]
  {
    :dead-zone   (params 2)
    :range-ratio (params 3)
  })

(defn int16le [^bytes data f t]
  (b| (aget data t) (b& (b< (aget data f) 8) 0xff)))

(defrecord imu-data [a r])

; https://github.com/dekuNukem/Nintendo_Switch_Reverse_Engineering/blob/master/imu_sensor_notes.md#convert-to-basic-useful-data-using-raw-values

(def imu-int16-acc-comp
  (double (/ 16000 65535 1000)))

(def imu-int16-gyr-comp
  (double (/ 4588 65535)))

; TODO find out what this is supposed to be
(def g 3.90631e-3)

(def imu-acc-scale
  (nn/dv imu-int16-acc-comp imu-int16-acc-comp imu-int16-acc-comp 1))

(def imu-gyr-scale
  (nn/dv imu-int16-gyr-comp imu-int16-gyr-comp imu-int16-gyr-comp 1))

(def g-mat
  (nn/dge 4 4 [1 0 0 0 0 1 0 0 0 0 1 0 0 0 g 1]))

; map cat mapcat filter remove take take-while take-nth drop drop-while replace
; partition-by partition-all keep keep-indexed map-indexed distinct interpose dedupe random-sample
;  𝌂   ⬇ ︎ ↓ 𝑓 𝒇  ▥ ⿲  ↓ ⏙ 𐄉 ∃ ⧢
; ⊂⊌⊍🄕⒡𝄑

(def E partition-all)
(def ⏙ partition-all)
(def 𝌂 partition-all)
(def ⬇ map)

(defn scale-imu-data [imu-data]
  (-> imu-data
    (update :a (fn [v] (nc/mv g-mat (nvm/mul imu-acc-scale v))))
    (update :r (fn [v] (nvm/mul imu-gyr-scale v)))))

(defn bytes-xf->imu-data [^bytes data]
  (comp
    (⬇ (fn [i] (int16le data i (+ 1 i))))
    (⏙ 3)
    (⬇ (fn [p] (apply nn/dv (concat p [1]))))
    (⏙ 2)
    (⬇ (fn [p] (apply (comp scale-imu-data ->imu-data) p)))))

(def report-xf->imu-data
  (mapcat
    (fn [{data :data}]
      (sequence
        (bytes-xf->imu-data data)
        (range 13 (+ 13 (* 12 3)) 2)))))

; https://github.com/dekuNukem/Nintendo_Switch_Reverse_Engineering/blob/master/imu_sensor_notes.md#packet-data-information
; Using all 3 samples let you have a 5ms precision
; Execution time mean : 5.355734 µs
(defn decode-imu-data [^bytes data]
  (sequence (bytes-xf->imu-data data)
    (range 13 (+ 13 (* 12 3)) 2)))

(defn add-imu-data-channel [{ic :input-report-channel :as j}]
  (let [dc (chan (dropping-buffer 16) report-xf->imu-data)]
    (pipe ic dc)
    (assoc j :data-channel dc :message-channel (chan))))

; get SPI dump
;(with-set-output-report jl (bytez zero16 {0 0x01 10 0x10 11 0x0 12 0x60 13 0x0 14 0x0 15 0x1d}))

; https://github.com/dekuNukem/Nintendo_Switch_Reverse_Engineering/blob/master/spi_flash_notes.md#x6000-factory-configuration-and-calibration
(defn decode-factory-config [reply-data]
  (let [data (subvec reply-data 20)]
    {
     :pre-data      (subvec reply-data 0 32)
     :report-id     [(first reply-data)]
     :echo          (subvec reply-data 13 19)
     :data          data
     :mcu           (subvec data 0x20 0x38)
     :left-stick    (subvec data 0x3d 0x46)
     :right-stick   (subvec data 0x46 0x4f)
     :mcu-offsets   (subvec data 0x80 0x86)
     :stick-params1 (subvec data 0x86 0x98)
     :stick-params2 (subvec data 0x98 0xaa)
     }))

(defn decode-user-config [reply-data]
  (let [data (subvec reply-data 20)]
    {
     :pre-data      (subvec reply-data 0 32)
     :report-id     [(first reply-data)]
     :echo          (subvec reply-data 13 19)
     :data          data
     :left-stick    (subvec data 0x12 0x1b)
     :right-stick   (subvec data 0x1d 0x26)
     :mcu           (subvec data 0x28 0x40)
     }))

(defn on-input-report [{^HidDevice joycon :device :as j} f]
  (.setInputReportListener joycon
    (reify InputReportListener
     (onInputReport [this source reportID data reportLength]
      (f j reportID data reportLength)))))

(defn dont-listen [{^HidDevice joycon :device :as j}]
  (.setInputReportListener joycon nil)
  j)

(defn debug-stick-data! [{^HidDevice joycon :device stop-button :stop-button stop-button-index :stop-button-index stick-offsets :stick-offsets :as j}]
  (.setInputReportListener joycon
    (reify InputReportListener
     (onInputReport [this source reportID data reportLength]
       (println ">" (count data) (decode-stick-data stick-offsets data))
       (when (== stop-button (aget data stop-button-index))
         (.setInputReportListener joycon nil))))))

(defn debug-stick-data-async! [{^HidDevice joycon :device stick-offsets :stick-offsets
                               stop-button        :stop-button stop-button-index :stop-button-index
                               ic                 :input-report-channel data-channel :data-channel data-channel' :data-channel' :as j}]
  (set-output-report-preset! j :set-input-report-mode/standard)
  (<!! ic)
  (let [dc (chan)
        mc (chan)
        gc (go-loop []
             (let [^bytes data (:data (<! dc))]
               (>! data-channel {:stick (decode-stick-data stick-offsets data)})
               (if (== stop-button (aget data stop-button-index))
                 (>! mc :stop)
                 (recur))))
        debugging (go-loop [] (if-let [d (<! data-channel)] (do (println d) (recur)) :end))]
    (pipe ic dc)
    (go-loop []
      (let [m (<! mc)]
      (cond
       (= m :stop) (do (println "stop!" (<! debugging)) (close! dc) (close! gc) (close! ic) (close! mc) (close! data-channel) (close! debugging) :stopped)
        :otherwise (recur))))
    (assoc j :debug-channel dc :result-channel gc :message-channel mc)))

(defn debug! [{^HidDevice joycon :device si :stop-button-index sb :stop-button :as j}]
  (.setInputReportListener joycon
    (reify InputReportListener
     (onInputReport [this source reportID data reportLength]
      (println ">" (print-hex reportID) " : " (string/join " " (map print-hex data)))
      (when (== sb (aget data si))
        (.setInputReportListener joycon nil))))))

(defn debug-imu! [{^HidDevice joycon :device si :stop-button-index sb :stop-button :as j}]
  (.setInputReportListener joycon
    (reify InputReportListener
     (onInputReport [this source reportID data reportLength]
       (doseq [d (decode-imu-data data)]
         (println ">" d))
      (when (== sb (aget data si))
        (.setInputReportListener joycon nil))))))

(defn report-once [{^HidDevice joycon :device :as j} f]
  (.setInputReportListener joycon
    (reify InputReportListener
     (onInputReport [this source reportID data reportLength]
      (f j reportID data reportLength)
       (.setInputReportListener joycon nil)))))

(defn get-callibration! [{ic :input-report-channel :as j}]
  (let [
        _ (set-output-report-preset! j :set-input-report-mode/simple-hid)
        _ (<!! ic)
        _ (set-output-report-preset! j :spi-flash-read-factory-config)
        factory-config (decode-factory-config (vec (:data (<!! ic))))
        _ (set-output-report-preset! j :spi-flash-read-user-config)
        user-config (decode-user-config (vec (:data (<!! ic))))
        ]
    {
      :raw-factory factory-config
      :raw-user    user-config
      :factory
        {
          :left-stick  (decode-3x3 :left  (:left-stick factory-config))
          :right-stick (decode-3x3 :right (:right-stick factory-config))
          :left-stick-params  (decode-stick-params (:stick-params1 factory-config))
          :right-stick-params (decode-stick-params (:stick-params2 factory-config))
        }
      :user
        {
          :left-stick  (decode-3x3 :left  (:left-stick user-config))
          :right-stick (decode-3x3 :right (:right-stick user-config))
        }}))


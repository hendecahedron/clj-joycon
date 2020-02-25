### Clojure joycon library


WIP Clojure API for Joycons

```

(use 'joycon.core)
(use 'joycon.vibration)
(use :reload 'joycon.ui.ui)

; get a joycon (vibration & IMU enabled by default)
(def jl (joycon! :left)

; make it vibrate
(send-vibrations! jl [[150 160 0.79] [200 80 0.0]]

; see the raw stick data
(debug-stick-data! jl)

; see all the data
(debug! jl)

; example subcommands
(set-output-report-preset! jl :set-player-lights/on)

(set-output-report-preset! jl :set-player-lights/flash)

(set-output-report-preset! jl :set-player-lights/off)

; see a rough UI with some charts

(def j1 (debug-joycon! jr))

; stop UI

(undebug-joycon! j1)

; close it
(close-joycon! jl)

```

#### What's done

* Basic communication and bit-manipulation

* Basic control of vibration

#### To be done


* convenient API for communication using core.async

* Callibration of sensor data


Progress on this will be slow 


### References:


https://github.com/dekuNukem/Nintendo_Switch_Reverse_Engineering

https://github.com/elgoupil/joyconLib

https://github.com/Looking-Glass/JoyconLib

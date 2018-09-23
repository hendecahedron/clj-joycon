##Clojure joycon library


WIP Clojure API for Joycons

```

(use 'joycon.core)
(use 'joycon.vibration)

; get a joycon (vibration &amp; IMU enabled by default)
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

; close it
(close-joycon! jl)

```

#### What's done

* Basic communication and bit-manipulation

* Basic control of vibration

#### To be done


* convenient API for communication using core.async

* Callibration of sensor data


Progress on this will be slow as it's a 1-hour per weekend project 


##References:


https://github.com/dekuNukem/Nintendo_Switch_Reverse_Engineering

https://github.com/elgoupil/joyconLib

https://github.com/Looking-Glass/JoyconLib

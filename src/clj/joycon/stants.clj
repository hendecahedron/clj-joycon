(ns joycon.stants)

(def constants-map
  {:y               "Y"
   :right-stick-on  4
   :capture-off     -32
   :down            "DOWN"
   :down-off        -1
   :a-off           -8
   :r               "R"
   :home            "HOME"
   :sl-off          -32
   :sr-off          -16
   :left-stick-on   8
   :left-off        -8
   :right-stick     "RIGHT_STICK"
   :b-on            4
   :right-on        4
   :sl              "SL"
   :a-on            8
   :plus-off        -2
   :zl-off          -128
   :plus-on         2
   :minus-on        1
   :minus           "MINUS"
   :sl-on           32
   :left-on         8
   :y-off           -1
   :l               "L"
   :up              "UP"
   :joycon-left     (short 8198)
   :zr-on           128
   :sr-on           16
   :home-on         16
   :capture-on      32
   :right-off       -4
   :x-on            2
   :home-off        -16
   :up-on           2
   :minus-off       -1
   :plus            "PLUS"
   :r-off           -64
   :b               "B"
   :r-on            64
   :right           "RIGHT"
   :manufacturer    "Nintendo"
   :left-stick      "LEFT_STICK"
   :l-on            64
   :left-stick-off  -8
   :x               "X"
   :zl              "ZL"
   :down-on         1
   :capture         "CAPTURE"
   :x-off           -2
   :y-on            1
   :zr              "ZR"
   :up-off          -2
   :zr-off          -128
   :zl-on           128
   :l-off           -64
   :sr              "SR"
   :joycon-right    (short 8199)
   :a               "A"
   :b-off           -4
   :left            "LEFT"
   :right-stick-off -4
   :vendor-id       1406})

(defn defall []
  (doall (map (fn [[k v]] (eval `(def ~(symbol (name k)) ~v))) constants-map)))

(defall)
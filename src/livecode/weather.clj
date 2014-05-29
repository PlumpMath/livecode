(ns workbook.weather
  (use quil.core))

(defn parse-fixed-width-fields [line cuts]
  (loop [in-str line cuts cuts out []]
    (if (seq cuts)
      (recur (subs in-str (first cuts)) (rest cuts) (conj out (clojure.string/trim (subs in-str 0 (first cuts)))))
      out)))

(defn parse-weather-station [line]
  (zipmap [:usaf :wban :station-name :country :state :call-sign :lat :long :elevation :begin :end]
          (parse-fixed-width-fields line [7 6 30 3 6 6 7 8 10 9 8])))

(defn load-weather-stations [filename]
  (reduce #(assoc %1 (str (:usaf %2) "-" (:wban %2)) %2) {}
          (map parse-weather-station (drop 22 (clojure.string/split-lines (slurp filename))))))

(def weather-stations (load-weather-stations "data/ish-history.txt"))

(defn load-weather-file [filename]
  (rest (for [line (clojure.string/split-lines (slurp filename))]
          ;;  slp-a slp-b stp-a stp-b visibility-min visibility-max windspeed-min windspeed-max maxspeed gust max min precipitation sndp frshtt
          (let [[usaf wban ymd-date temp temp-time dew-point dew-time & junk] (clojure.string/split line #"[ ]+")]
            {:usaf usaf :wban wban :ymd-date ymd-date :temp temp :dew-point dew-point}))))

(def weather-files
  (filter #(.contains % "op") (map str (file-seq (clojure.java.io/file "data/weather")))))

(defn get-weather-station [records]
  (let [sample (first records)]
    (weather-stations (str (sample :usaf) "-" (sample :wban)))))

(def all-records
  (remove (comp nil? get-weather-station) (pmap load-weather-file weather-files)))

(defn f-to-c
  "I can't believe weather stations use the units of perfidy!"
  [f]
  (float (* (- f 32) 5/9)))

(defn string-to-lat-or-long [s]
  (let [len (count s)]
    (if (> 3 len)
      -999999
      (read-string (str (subs s 0 (- len 3)) "." (subs s (- len 3)))))))

(def all-temps
  (for [records all-records]
    (let [station (get-weather-station records)
          temps   (map (comp f-to-c read-string :temp) records)
          dews    (map (comp #(if (> % 100) 0 %)
                        f-to-c read-string :dew-point) records)]
      (list (apply min temps) (apply max temps) (apply max dews)
            (station :station-name) (station :country)
            (string-to-lat-or-long (station :lat)) (string-to-lat-or-long (station :long))))))

;; (filter #(.contains (nth % 3) "BERLIN") all-temps)

;;(count all-temps)
;; => 12089

;;(apply min (map first all-temps))
;; => -79.27778

;;(apply max (map second all-temps))
;; => 43.22222

(def best-weather
  (filter (fn [[low high dew station-name country lat long]]
            (and (not= country "")
                 (not= lat -999999)
                 (> low 0)
                 (< lat 64)  (> lat 24)
                 (< long 40) (> long -40) 
                 (< high 29) (> high 20)
                 (< dew 23)  (> dew 9))) all-temps))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; whole planet
(def min-long -180)
(def max-long  180)
(def max-lat  -90)
(def min-lat   90)

(defn shortest-day [lat]
  (- 12 (* (abs lat) (/ 12 90))))

;; western eurasia
(def min-long -40)
(def max-long  40)
(def max-lat   24)
(def min-lat   64)

(defn setup []
  (frame-rate 10))

(defn draw []
  (smooth)
  (background 0x181818)
  (stroke-weight 3)
  (no-fill)
  (stroke 60)
  (doseq [[_ _ _ _ _ lat long] all-temps]
    (point (map-range long min-long max-long 0 (width))
           (map-range lat  min-lat  max-lat  0 (height))))
  (stroke 255)
  (doseq [[_ _ _ _ _ lat long] best-weather]
    (point (map-range long min-long max-long 0 (width))
           (map-range lat  min-lat  max-lat  0 (height)))))

;; this will start the sketch
(defsketch example
  :title "Mapping the good weather"
  :setup setup
  :draw draw
  :size [800 500])

(ns livecode.plotting
  (:require [clojure.data.json :as json]
            [clj-time.core :as ct]
            [clj-time.format :as cf])
  (:use [incanter core stats charts io])
  (:use [livecode.core]))

;; view a function-plot
(view (function-plot sin -4 4))

;; view a histogram of the normal distribution
(view (histogram (sample-normal 1000)))

;; an example scatter plot based on our data
(def plot (scatter-plot (vals inequality) (map (comp second expectancy) (keys inequality))))

(view plot)

;; add text labels so we know which countries are which
(doseq [k (keys inequality)]
  (add-text plot (inequality k) (second (expectancy k)) k))

;; how about a function to parameterize the graphs?
(defn ineq-vs-expect [col]
  (let [plot (scatter-plot (vals inequality) (map (comp col expectancy) (keys inequality)))]
    ;; add text labels so we know which countries are which
    (doseq [k (keys inequality)]
      (add-text plot (inequality k) (col (expectancy k)) k))
    (view plot)))

;; note: the parameter is just a function
(ineq-vs-expect first)
(ineq-vs-expect second)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; and we're not stuch with R's look

;; ;; some jfree.chart junk so I can over-ride the default theme
;; (import '(org.jfree.chart StandardChartTheme)
;;         '(org.jfree.chart.plot DefaultDrawingSupplier)
;;         '(java.awt Color))

;; ;; behind the grid       (getChartBackgroundPaint [] (Color/getHSBColor 42 0.27 0.75))

;; (def jacks-theme
;;   (doto (StandardChartTheme/createJFreeTheme)
;;     (.setPlotBackgroundPaint (Color. 228 223 210))
;;     (.setDrawingSupplier
;;      (proxy [DefaultDrawingSupplier] []
;;        (getNextPaint [] (Color/getHSBColor (* (rand) 255) 0.7 0.7))))))

;; (set-theme plot jacks-theme)



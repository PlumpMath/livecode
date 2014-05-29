(ns livecode.core
  (:use [seesaw core font graphics])
  (:require [seesaw.bind :as b]
            [seesaw.mouse :as m]))

(native!)

(def f (frame :title "Animation test"))

(-> f pack! show!)

(defn display [content]
  (config! f :content content)
  content)

(defn paint-ball [c g]
  (let [w (.getWidth c)
        h (.getHeight c)
        [x y] (m/location)
        ball-color  (seesaw.color/color "#000066")]
    (anti-alias g)
    (draw g
          (circle x y 20)
          (style :foreground ball-color
                 :background ball-color))))

(display (border-panel
          :center (canvas :id :animatron
                          :background "#eeeeee"
                          :paint paint-ball)))

(defn mouse-moved [e]
  (repaint! f))

(listen f :mouse-moved mouse-moved)

(require '[clojure.data.json :as json])

(def fen-tweets
  (remove nil?
          (distinct
           (for [tweet (flatten feneon)]
             (when tweet (tweet "text"))))))

;;(spit "feneon.txt" (pr-str fen-tweets))


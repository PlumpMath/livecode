
;; follows redirects
(defn fetch-page [url]
  (enlive/html-resource
   (loop [final-url (java.net.URL. url)]
     (let [conn (.openConnection final-url)]
       (if (re-find #"30[1-3]" (.getHeaderField conn 0))
         (recur (java.net.URL. (.getHeaderField conn "location")))
         final-url)))))

;; grab page title
(first (enlive/select (fetch-page "http://google.com") [:title enlive/text]))

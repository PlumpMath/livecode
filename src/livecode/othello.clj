(ns livecode.othello
  (:require [net.cgrand.enlive-html :as enlive]))

;; consider:
;; https://github.com/kyleburton/clj-xpath

(def othello
  (enlive/xml-resource (java.net.URL. "file:/Users/jack/tmp/othello.xml")))

;; select every document, count how many documents are in there
(count (enlive/select othello [:eblacorpus :documents :document]))
;; => 39

;; get the names of the documents
(map #(-> % :attrs :name)
     (enlive/select othello [:eblacorpus :documents :document]))
;; => ("" "Baudissin (edited by Bab and Levy)" "Baudissin (edited by Brunner)" "Baudissin (edited by Mommsen)" "Baudissin (edited by Wenig)" "Baudissin (edited by Wolff)" "Benda (1826)" "Bodenstedt" "Boito (translated by Felsenstein and Stueber)" "Bolte and Hamblock" "Buhss" "Bärfuß" "Engel" "Engler" "Eschenburg (edited by Eckert)" "Flatter" "Fried" "Gildemeister" "Gundolf" "Günther" "Karbus" "Laube" "Lauterbach and Gleisberg" "Leonard" "Motschach" "Ortlepp" "Rothe" "Rüdiger" "Schaller" "Schiller and Voss" "Schröder" "Schwarz" "Swaczynna" "Vischer" "Wachsmann" "Wieland" "Zaimoglu and Senkel" "Zeynek" "Zimmer")

;; how many of these labeled chunks are there?
(count (map #(-> % :attrs :data-eblasegid)
            (enlive/select othello [(enlive/attr? :data-eblasegid)])))
;; => 11520

;; hm, there occur in start/end pairs, how many distinct ones?
(count (distinct (map #(-> % :attrs :data-eblasegid)
                      (enlive/select othello [(enlive/attr? :data-eblasegid)]))))
;; => 5760 over the whole corpus

;; in the first document?
(def first-document (first (enlive/select othello [:eblacorpus :documents :document])))

(count (distinct (enlive/select first-document [(enlive/attr? :data-eblasegid)])))
;; => 320

;; just pull out all the text and use the positional segment data to
;; look up chunks
(def text-content
  (clojure.string/replace
   (apply str (map enlive/text (enlive/select first-document [:doccontent])))
   #"[\[\]]" ""))

;; get the english segment definitions
(def english-segments
  (map
   #(merge (reduce (fn [a [k v]] (assoc a k (read-string v))) {} (% :attrs))
           (-> % :content second :content second :attrs))
   (enlive/select first-document [:segmentdefinition])))

(map (fn [segdef] (mapcat #(-> % :attrs) (enlive/select segdef [:segmentattribute])))
     (take 10 (enlive/select first-document [:segmentdefinition])))

(take 5 (drop 5 (sort-by :startpos english-segments)))
;; => ({:attribname "type", :attribval "Speech", :id 38077, :startpos 179, :length 76} {:attribname "type", :attribval "Speech", :id 38078, :startpos 269, :length 30} {:attribname "type", :attribval "Speech", :id 38079, :startpos 313, :length 200} {:attribname "type", :attribval "Speech", :id 38561, :startpos 471, :length 42} {:attribname "type", :attribval "Speech", :id 38080, :startpos 527, :length 124})

;; combine segment and text data
(map #(vector (subs text-content (% :startpos) (+ (% :startpos) (% :length)))
              (keyword (.toLowerCase (% :attribval))))
     (take 15 (filter :attribval (sort-by :startpos english-segments))))
;; => (["Act I, Scene 3" :s.d.] ["A council-chamber." :s.d.] ["The DUKE and Senators sitting at a table; Officers attending" :s.d.] ["There is no composition in these newsThat gives them credit." :speech] ["Indeed, they are disproportioned;My letters say a hundred and seven galleys." :speech] ["And mine, a hundred and forty." :speech] ["And mine, two hundred:But though they jump not on a just account, -As in these cases, where the aim reports,'Tis oft with difference - yet do they all confirmA Turkish fleet, and bearing up to Cyprus." :speech] ["A Turkish fleet, and bearing up to Cyprus." :speech] ["Nay, it is possible enough to judgment:I do not so secure me in the error,But the main article I do approveIn fearful sense." :speech] ["Within " :s.d.] ["What, ho! what, ho! what, ho!" :speech] ["A messenger from the galleys. " :speech] ["Enter a Sailor" :s.d.] ["Now, what's the business?" :speech] ["The Turkish preparation makes for Rhodes;So was I bid report here to the stateBy signior Angelo." :speech])

(def baudissin-segments
  (map
   #(merge (% :attrs) (-> % :content second :content second :attrs))
   (enlive/select (second (enlive/select othello [:eblacorpus :documents :document :segmentdefinitions])) [:segmentdefinition])))

(take 5 (sort-by :startpos baudissin-segments))
;; => ({:attribname "type", :attribval "S.D.", :length "12", :startpos "0", :id "38182"} {:attribname "type", :attribval "S.D.", :length "104", :startpos "12", :id "38183"} {:attribname "type", :attribval "Speech", :length "58", :startpos "121", :id "38184"} {:attribname "type", :attribval "Speech", :length "82", :startpos "194", :id "38185"} {:attribname "type", :attribval "Speech", :length "26", :startpos "281", :id "38186"})

;; we're going to need to know the alignments. let's get the first one
;; for each document to see what they look like
(map #(-> (enlive/select % [:alignment]) first :attrs :id)
     (enlive/select othello [:eblacorpus :documents :document]))

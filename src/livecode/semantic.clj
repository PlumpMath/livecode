(ns livecode.semantic
  (:require [net.cgrand.enlive-html :as enlive]
            [clj-time.core :as ct]
            [clj-time.format :as cf]
            [clojure.string :as cs]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; knowledge representation, syllogism via knowledge base
;; there are good, fast libraries for this sort of thing, but we're going
;; to do this from scratch for clarity

(defrecord Triple [subject predicate object])

(def knowledge-base
  [(Triple. :Socrates    :is-a         :Greek)
   (Triple. :Greek       :is-a         :human-being)
   (Triple. :human-being :has-property :mortal)])

(defn collect-predicate-along-axis [kb subject predicate axis]
  (let [facts (filter #(= subject (:subject %)) kb)
        arcs  (filter #(= axis (:predicate %)) facts)]
    (concat
     (filter #(= predicate (:predicate %)) facts)
     (mapcat #(collect-predicate-along-axis kb (:object %) predicate axis) arcs))))

(defn get-properties [subject kb]
  (map :object (collect-predicate-along-axis kb subject :has-property :is-a)))

(get-properties :Socrates knowledge-base)
;; => (:mortal)

(def knowledge-base
  [(Triple. :Berlin       :is-a      :city)
   (Triple. :Berlin       :part-of   :Germany)
   (Triple. :Germany      :part-of   :Mitteleuropa)
   (Triple. :Germany      :member-of :EU)
   (Triple. :Germany      :member-of :NATO)
   (Triple. :Mitteleuropa :part-of   :Eurasia)
   (Triple. :Eurasia      :part-of   :Earth)])

(defn part-of-what? [subject kb]
  (map :object (collect-predicate-along-axis kb subject :part-of :part-of)))

(part-of-what? :Berlin knowledge-base)
;; =>(:Germany :Mitteleuropa :Eurasia :Earth)

;; find :part-of and :member-of relations for :Berlin by along the :part-of axis
(reduce #(assoc %1 %2 (map :object (collect-predicate-along-axis knowledge-base :Berlin %2 :part-of))) {} [:part-of :member-of])
;; => {:member-of (:EU :NATO), :part-of (:Germany :Mitteleuropa :Eurasia :Earth)}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some data from dbpedia

;; N.B. this code is meant to help one understand the concepts
;; involved -- if one were to put something like this in production,
;; it'd be much better to use one of the many excellent open source
;; libraries available for this purpose.

;; semantic web data is often formatted as RDF/XML, but I think that
;; format is horrible, so I generally prefer to use something like
;; n-triples (N3):
;; https://dvcs.w3.org/hg/rdf/raw-file/default/rdf-turtle/n-triples.html

;; for the sake of this gist we'll limit ourselves to the flavor of N3
;; produced by dbpedia

(def fetch-n3
  "Fetch N3 data for a given dbpedia resource. Expects a URL like
'http://cs.dbpedia.org/resource/Otto_Lilienthal', which is
re-written to a dbpedia N3 data URL like
'http://cs.dbpedia.org/data/Otto_Lilienthal.n3'. Results are
memoized to speed up repeated requests."
  (memoize #(slurp (str (cs/replace % "resource" "data") ".n3"))))

;; this is a very simple parser, it has bugs galore
(defn parse-n3
  "Parse the flavor of N3 returned by dbpedia into a vector of vectors
of strings representing the triples in the original N3. Namespaces
are automatically expanded at parse time."
  [n3]
  (loop [triples (map #(subs % 0 (- (count %) 2)) ;; remove comments and trim junk
                      (remove #(re-find #"^#" %) (cs/split-lines n3)))
         old-s nil
         old-p nil
         prefixes {"http:" "http:"} ;; prefix replacements include http: -> http:
         out []]
    (if (seq triples)
      (let [[s p o] (let [chunks (map #(if (= (first %) \<) (subs % 1 (- (count %) 1)) %)
                                      (cs/split (first triples) #"\t"))]
                      (if (= 2 (count chunks)) ;; duples = prefix definitions,
                        chunks ;; skip prefix expansion on them
                        (map #(if-let [[_ pre post] (re-find #"^([a-z0-9\-]*:)(.*)" %)]
                                (str (prefixes pre) post) ;; expand prefixes
                                %)
                             chunks)))
            s       (if (= s "") old-s s) ;; empty s or p = carryover most recent
            p       (if (= p "") old-p p)] ;; s/p value
        (if-let [[_ prefix] (re-find #"@prefix (.*)" s)]
          (recur (rest triples) s p (assoc prefixes prefix p) out)
          (recur (rest triples) s p prefixes (conj out [s p o]))))
      out)))

(def james-joyce
  (parse-n3 (fetch-n3 "http://dbpedia.org/resource/James_Joyce")))

(count james-joyce)
;; => 689 (nearly 700 assertions about the late Stephen Daedalus!)

;; let's look at a few from the middle:
(take 5 (drop 342 james-joyce))
;; (["http://dbpedia.org/resource/James_Joyce"
;;   "http://dbpedia.org/property/id"
;;   "2467"]
;;  ["http://dbpedia.org/resource/James_Joyce"
;;   "http://dbpedia.org/property/placeOfBirth"
;;   "\"Rathgar, Dublin, Ireland\"@en"]
;;  ["http://dbpedia.org/resource/James_Joyce"
;;   "http://dbpedia.org/property/shortDescription"
;;   "\"Irish novelist and poet\"@en"]
;;  ["http://dbpedia.org/resource/James_Joyce"
;;   "http://dbpedia.org/property/author"
;;   "\"yes\"@en"]
;;  ["http://dbpedia.org/resource/James_Joyce"
;;   "http://xmlns.com/foaf/0.1/surname"
;;   "\"Joyce\"@en"])

;; it's pretty easy to pull the subjects, predicates or objects out of
;; this data structure
(defn get-subjects [n3]
  (distinct (map first n3)))

(defn get-predicates [n3]
  (distinct (map second n3)))

(defn get-objects [n3]
  (distinct (map last n3)))

;; so what kinds of things to do we know about JJ?

(get-predicates james-joyce)
;; ("http://www.w3.org/2002/07/owl#sameAs"
;;  "http://dbpedia.org/property/influences"
;;  "http://dbpedia.org/ontology/influencedBy"
;;  "http://dbpedia.org/ontology/influenced"
;;  "http://dbpedia.org/property/influenced"
;;  "http://dbpedia.org/ontology/author"
;;  "http://dbpedia.org/property/author"
;;  "http://xmlns.com/foaf/0.1/primaryTopic"
;;  "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
;;  "http://purl.org/dc/terms/subject"
;;  "http://www.w3.org/2000/01/rdf-schema#label"
;;  "http://xmlns.com/foaf/0.1/depiction"
;;  "http://www.w3.org/2000/01/rdf-schema#comment"
;;  "http://xmlns.com/foaf/0.1/name"
;;  "http://dbpedia.org/ontology/wikiPageID"
;;  "http://dbpedia.org/ontology/wikiPageRevisionID"
;;  "http://xmlns.com/foaf/0.1/givenName"
;;  "http://xmlns.com/foaf/0.1/isPrimaryTopicOf"
;;  "http://purl.org/dc/elements/1.1/description"
;;  "http://www.w3.org/ns/prov#wasDerivedFrom"
;;  "http://dbpedia.org/ontology/abstract"
;;  "http://dbpedia.org/ontology/thumbnail"
;;  "http://dbpedia.org/ontology/wikiPageExternalLink"
;;  "http://dbpedia.org/property/name"
;;  "http://dbpedia.org/property/wordnet_type"
;; [...]

;; wordnet_type is one of the predicates! let's see what synset is assigned
(filter (comp (partial = "http://dbpedia.org/property/wordnet_type") second) james-joyce)
;; => (["http://dbpedia.org/resource/James_Joyce" "http://dbpedia.org/property/wordnet_type" "http://www.w3.org/2006/03/wn/wn20/instances/synset-writer-noun-1"])
;; ... seems sensible

;; how about multiple semantically similar predicates?
(filter (partial re-find #"influence") (get-predicates james-n3))
;; => ("http://dbpedia.org/property/influences" "http://dbpedia.org/ontology/influencedBy" "http://dbpedia.org/ontology/influenced" "http://dbpedia.org/property/influenced" "http://dbpedia.org/property/influencedBy")

;; can we put them into a set and fetch all matches and group them?
(group-by second (filter (comp (set (filter (partial re-find #"influence") (get-predicates james-n3))) second) james-joyce))
;; {"http://dbpedia.org/property/influences"
;;  [["http://dbpedia.org/resource/Donald_Barthelme"
;;    "http://dbpedia.org/property/influences"
;;    "http://dbpedia.org/resource/James_Joyce"]
;;   ["http://dbpedia.org/resource/John_Barth"
;;    "http://dbpedia.org/property/influences"
;;    "http://dbpedia.org/resource/James_Joyce"]
;; [...]

;;  "http://dbpedia.org/ontology/influencedBy"
;;  [["http://dbpedia.org/resource/Donald_Barthelme"
;;    "http://dbpedia.org/ontology/influencedBy"
;;    "http://dbpedia.org/resource/James_Joyce"]
;;   ["http://dbpedia.org/resource/John_Barth"
;;    "http://dbpedia.org/ontology/influencedBy"
;;    "http://dbpedia.org/resource/James_Joyce"]
;; [...]

;;  [["http://dbpedia.org/resource/Jens_Peter_Jacobsen"
;;    "http://dbpedia.org/ontology/influenced"
;;    "http://dbpedia.org/resource/James_Joyce"]
;;   ["http://dbpedia.org/resource/George_Moore_(novelist)"
;; [...]

;;  "http://dbpedia.org/property/influenced"
;;  [["http://dbpedia.org/resource/Jens_Peter_Jacobsen"
;;    "http://dbpedia.org/property/influenced"
;;    "http://dbpedia.org/resource/James_Joyce"]
;;   ["http://dbpedia.org/resource/George_Moore_(novelist)"
;;    "http://dbpedia.org/property/influenced"
;;    "http://dbpedia.org/resource/James_Joyce"]
;; [...]

;;  "http://dbpedia.org/property/influencedBy"
;;  [["http://dbpedia.org/resource/Joshua_Cohen_(writer)"
;;    "http://dbpedia.org/property/influencedBy"
;;    "http://dbpedia.org/resource/James_Joyce"]]}

;; Ja, not bad.

;; a general query function, pulls all triples from the knowledge base
;; that match the prototype triple supplied (nil for "anything goes"
(defn query-triples [triples qs qp qo]
  (filter (fn [[s p o]]
            (and (or (nil? qs) (= qs s))
                 (or (nil? qp) (= qp p))
                 (or (nil? qo) (= qo o)))) triples))

(map first (query-triples james-joyce nil "http://dbpedia.org/property/influenced" "http://dbpedia.org/resource/James_Joyce"))
;; ("http://dbpedia.org/resource/Jens_Peter_Jacobsen"
;;  "http://dbpedia.org/resource/George_Moore_(novelist)"
;;  "http://dbpedia.org/resource/William_Thomas_Stead"
;;  "http://dbpedia.org/resource/Fran%C3%A7ois_Rabelais"
;;  "http://dbpedia.org/resource/Mikhail_Lermontov"
;;  "http://dbpedia.org/resource/Stendhal"
;;  "http://dbpedia.org/resource/Sheridan_Le_Fanu"
;;  "http://dbpedia.org/resource/Joel_Chandler_Harris"
;;  "http://dbpedia.org/resource/Jonathan_Swift"
;;  "http://dbpedia.org/resource/Giordano_Bruno"
;;  "http://dbpedia.org/resource/Anton_Chekhov"
;;  "http://dbpedia.org/resource/Lord_Byron"
;;  "http://dbpedia.org/resource/Giambattista_Vico"
;;  "http://dbpedia.org/resource/Oscar_Wilde"
;;  "http://dbpedia.org/resource/Otto_Weininger"
;;  "http://dbpedia.org/resource/Maimonides")

;; how about something older
(def homer
  (parse-n3 (fetch-n3 "http://dbpedia.org/resource/Homer")))

(count homer)
;; => 468

(map first (query-triples authors nil "http://dbpedia.org/ontology/influencedBy" "http://dbpedia.org/resource/Homer"))
;; ("http://dbpedia.org/resource/Lucian"
;;  "http://dbpedia.org/resource/Benjamin_Fondane"
;;  "http://dbpedia.org/resource/Simone_Weil"
;;  "http://dbpedia.org/resource/Geet_Chaturvedi"
;;  "http://dbpedia.org/resource/Ovid"
;; [...]

;; and if we combine them we have a bigger knowledge base
(def authors (concat james-joyce homer))

;; which authors were influenced by both?
((group-by second (frequencies (map first (query-triples authors nil "http://dbpedia.org/ontology/influencedBy" nil)))) 2)
;; [["http://dbpedia.org/resource/Jorge_Luis_Borges" 2]
;;  ["http://dbpedia.org/resource/Anthony_Burgess" 2]]


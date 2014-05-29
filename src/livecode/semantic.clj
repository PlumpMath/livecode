(ns livecode.semantic
  (:require [net.cgrand.enlive-html :as enlive]
            [clj-time.core :as ct]
            [clj-time.format :as cf]))

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
;; => (:Germany :EU :Eurasia :Earth)

;; find :part-of and :member-of relations for :Berlin by along the :part-of axis
(reduce #(assoc %1 %2 (map :object (collect-predicate-along-axis knowledge-base :Berlin %2 :part-of))) {} [:part-of :member-of])
;; => {:member-of (:EU :NATO), :part-of (:Germany :Mitteleuropa :Eurasia :Earth)}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some data from dbpedia

(def portishead
  (enlive/xml-resource (java.net.URL. "http://dbpedia.org/data/Portishead_(band).rdf")))

;; let's have a look in there
(map #(-> % :attrs :rdf:about)
     (enlive/select portishead [:* (enlive/has [:dbpedia-owl:associatedMusicalArtist])]))

;; OK, so let's make a function
(defn get-objects-by-predicate [subject predicate]
  (map #(-> % :attrs :rdf:about)
       (enlive/select subject [:* (enlive/has [predicate])])))

(get-objects-by-predicate (fetch-dbpedia-rdf "http://dbpedia.org/resource/Portishead_(band)") :dbpedia-owl:associatedMusicalArtist)

;; we'll be going over the same data, so we'll memoize the fetch
(def fetch-dbpedia-rdf
  (memoize #(enlive/xml-resource (java.net.URL. (str (clojure.string/replace % "resource" "data") ".rdf")))))

;; now let's follow some links
(map #(get-objects-by-predicate (fetch-dbpedia-rdf %) :dbpedia-owl:associatedMusicalArtist)
     (get-objects-by-predicate (fetch-dbpedia-rdf "http://dbpedia.org/resource/Portishead_(band)") :dbpedia-owl:associatedMusicalArtist))

;; (("http://dbpedia.org/resource/Massive_Attack" "http://dbpedia.org/resource/Portishead_(band)" "http://dbpedia.org/resource/CirKus" "http://dbpedia.org/resource/Tricky" "http://dbpedia.org/resource/Tunde_Adebimpe" "http://dbpedia.org/resource/Guy_Garvey" "http://dbpedia.org/resource/Martina_Topley-Bird" "http://dbpedia.org/resource/Andrew_Vowles" "http://dbpedia.org/resource/Elizabeth_Fraser" "http://dbpedia.org/resource/Horace_Andy" "http://dbpedia.org/resource/Shara_Nelson" "http://dbpedia.org/resource/Mos_Def" "http://dbpedia.org/resource/Marc_Marot" "http://dbpedia.org/resource/Hope_Sandoval" "http://dbpedia.org/resource/Jerry_Fuchs" "http://dbpedia.org/resource/Matt_Schwartz" "http://dbpedia.org/resource/Nellee_Hooper" "http://dbpedia.org/resource/Deadly_Apples" "http://dbpedia.org/resource/Peeping_Tom_(band)" "http://dbpedia.org/resource/Burial_(musician)" "http://dbpedia.org/resource/Damon_Albarn" "http://dbpedia.org/resource/Daddy_G" "http://dbpedia.org/resource/Robert_Del_Naja" "http://dbpedia.org/resource/The_Blue_Aeroplanes" "http://dbpedia.org/resource/Unkle" "http://dbpedia.org/resource/Dot_Allison" "http://dbpedia.org/resource/Haydn_Bendall" "http://dbpedia.org/resource/Stephanie_Dosen" "http://dbpedia.org/resource/Lendi_Vexer" "http://dbpedia.org/resource/Mark_'Spike'_Stent") ("http://dbpedia.org/resource/Massive_Attack" "http://dbpedia.org/resource/Portishead_(band)" "http://dbpedia.org/resource/CirKus" "http://dbpedia.org/resource/Get_the_Blessing" "http://dbpedia.org/resource/Beth_Gibbons" "http://dbpedia.org/resource/Geoff_Barrow" "http://dbpedia.org/resource/Joe_Volk" "http://dbpedia.org/resource/Lendi_Vexer" "http://dbpedia.org/resource/Black_Submarine") ("http://dbpedia.org/resource/CirKus") ("http://dbpedia.org/resource/Portishead_(band)" "http://dbpedia.org/resource/Get_the_Blessing") ("http://dbpedia.org/resource/Beth_Gibbons" "http://dbpedia.org/resource/Paul_Webb") ("http://dbpedia.org/resource/Stephanie_McKay" "http://dbpedia.org/resource/Geoff_Barrow") ("http://dbpedia.org/resource/Joe_Volk") ("http://dbpedia.org/resource/Lovage_(band)" "http://dbpedia.org/resource/Lendi_Vexer") ("http://dbpedia.org/resource/The_Verve" "http://dbpedia.org/resource/Nick_McCabe" "http://dbpedia.org/resource/Simon_Jones_(musician)" "http://dbpedia.org/resource/Black_Submarine"))

(defn get-resources-by-predicate [subject predicate]
  (map #(-> % :attrs :rdf:resource) (enlive/select subject [predicate])))

(let [bands (distinct (mapcat #(get-objects-by-predicate (fetch-dbpedia-rdf %) :dbpedia-owl:associatedMusicalArtist)
                              (get-objects-by-predicate (fetch-dbpedia-rdf "http://dbpedia.org/resource/Portishead_(band)") :dbpedia-owl:associatedMusicalArtist)))]
  (mapcat #(get-resources-by-predicate (fetch-dbpedia-rdf %) :dbpedia-owl:bandMember) bands))

;;("http://dbpedia.org/resource/Daddy_G" "http://dbpedia.org/resource/Robert_Del_Naja" "http://dbpedia.org/resource/Geoff_Barrow" "http://dbpedia.org/resource/Beth_Gibbons" "http://dbpedia.org/resource/Portishead_(band)" "http://dbpedia.org/resource/Neneh_Cherry" "http://dbpedia.org/resource/Cameron_McVey" "http://dbpedia.org/resource/Tunde_Adebimpe" "http://dbpedia.org/resource/Guy_Garvey" "http://dbpedia.org/resource/Mos_Def" "http://dbpedia.org/resource/Mos_Def" "http://dbpedia.org/resource/Hope_Sandoval" "http://dbpedia.org/resource/Hope_Sandoval" "http://dbpedia.org/resource/Nellee_Hooper" "http://dbpedia.org/resource/Mike_Patton" "http://dbpedia.org/resource/Damon_Albarn" "http://dbpedia.org/resource/Damon_Albarn" "http://dbpedia.org/resource/Daddy_G" "http://dbpedia.org/resource/Robert_Del_Naja" "http://dbpedia.org/resource/Rita_Lynch" "http://dbpedia.org/resource/Gerard_Starkie" "http://dbpedia.org/resource/The_Blue_Aeroplanes" "http://dbpedia.org/resource/Angelo_Bruschini" "http://dbpedia.org/resource/James_Lavelle" "http://dbpedia.org/resource/Pablo_Clements" "http://dbpedia.org/resource/Diego_Gui%C3%B1azu_DG" "http://dbpedia.org/resource/Natalie_Naveira" "http://dbpedia.org/resource/Beth_Gibbons" "http://dbpedia.org/resource/Geoff_Barrow" "http://dbpedia.org/resource/Geoff_Barrow" "http://dbpedia.org/resource/Simon_Jones_(musician)" "http://dbpedia.org/resource/Davide_Rossi" "http://dbpedia.org/resource/Nick_McCabe" "http://dbpedia.org/resource/Jennifer_Charles" "http://dbpedia.org/resource/Dan_the_Automator" "http://dbpedia.org/resource/Kid_Koala" "http://dbpedia.org/resource/Mike_Patton" "http://dbpedia.org/resource/Nick_McCabe" "http://dbpedia.org/resource/Simon_Jones_(musician)" "http://dbpedia.org/resource/Simon_Jones_(musician)")

;; who influenced James Joyce?
(get-objects-by-predicate (fetch-dbpedia-rdf "http://dbpedia.org/resource/James_Joyce") :dbpedia-owl:influenced)

;; ("http://dbpedia.org/resource/Giambattista_Vico" "http://dbpedia.org/resource/Jonathan_Swift" "http://dbpedia.org/resource/Maimonides" "http://dbpedia.org/resource/Oscar_Wilde" "http://dbpedia.org/resource/Laurence_Sterne" "http://dbpedia.org/resource/Anton_Chekhov" "http://dbpedia.org/resource/Fran%C3%A7ois_Rabelais" "http://dbpedia.org/resource/Mikhail_Lermontov" "http://dbpedia.org/resource/Stendhal" "http://dbpedia.org/resource/Sheridan_Le_Fanu" "http://dbpedia.org/resource/Jens_Peter_Jacobsen" "http://dbpedia.org/resource/George_Moore_(novelist)" "http://dbpedia.org/resource/Joel_Chandler_Harris" "http://dbpedia.org/resource/Otto_Weininger")

;; who was influenced *by* James Joyce?
(get-objects-by-predicate (fetch-dbpedia-rdf "http://dbpedia.org/resource/James_Joyce") :dbpedia-owl:influencedBy)

;;("http://dbpedia.org/resource/Brendan_Behan" "http://dbpedia.org/resource/Brian_O'Nolan" "http://dbpedia.org/resource/Colum_McCann" "http://dbpedia.org/resource/Donald_Barthelme" "http://dbpedia.org/resource/Gene_Wolfe" "http://dbpedia.org/resource/Giannina_Braschi" "http://dbpedia.org/resource/Hal_Duncan" "http://dbpedia.org/resource/Jerome_Charyn" "http://dbpedia.org/resource/John_Barth" "http://dbpedia.org/resource/Simon_Vestdijk" "http://dbpedia.org/resource/William_Styron" "http://dbpedia.org/resource/Camilo_Jos%C3%A9_Cela" "http://dbpedia.org/resource/Saul_Bellow" "http://dbpedia.org/resource/Virginia_Woolf" "http://dbpedia.org/resource/William_Faulkner" "http://dbpedia.org/resource/Bernard_Benstock" "http://dbpedia.org/resource/Mitch_Berman" "http://dbpedia.org/resource/Paul_Auster" "http://dbpedia.org/resource/Ronald_Verlin_Cassill" "http://dbpedia.org/resource/Alexander_Laurence" "http://dbpedia.org/resource/Andr%C3%A9_Aciman" "http://dbpedia.org/resource/Aris_Marangopoulos" "http://dbpedia.org/resource/Dylan_Thomas" "http://dbpedia.org/resource/Edna_O'Brien" "http://dbpedia.org/resource/Horia_G%C3%A2rbea" "http://dbpedia.org/resource/Irvine_Welsh" "http://dbpedia.org/resource/Jamie_O'Neill" "http://dbpedia.org/resource/Janet_Frame" "http://dbpedia.org/resource/Julien_Green" "http://dbpedia.org/resource/Lojze_Kova%C4%8Di%C4%8D" "http://dbpedia.org/resource/Luigi_Malerba" "http://dbpedia.org/resource/Malcolm_Lowry" "http://dbpedia.org/resource/Miho_Mosulishvili" "http://dbpedia.org/resource/Motojir%C5%8D_Kajii" "http://dbpedia.org/resource/Paul_Georgescu" "http://dbpedia.org/resource/Ralph_Ellison" "http://dbpedia.org/resource/Richard_Yates_(novelist)" "http://dbpedia.org/resource/Samuel_R._Delany" "http://dbpedia.org/resource/Subimal_Mishra" "http://dbpedia.org/resource/William_Michaelian" "http://dbpedia.org/resource/Marshall_McLuhan" "http://dbpedia.org/resource/Alice_Munro" "http://dbpedia.org/resource/Anthony_Burgess" "http://dbpedia.org/resource/B._S._Johnson" "http://dbpedia.org/resource/Bonnie_Bluh" "http://dbpedia.org/resource/Bret_Easton_Ellis" "http://dbpedia.org/resource/Don_DeLillo" "http://dbpedia.org/resource/Felix_Aderca" "http://dbpedia.org/resource/Harold_Bloom" "http://dbpedia.org/resource/Harold_Pinter" "http://dbpedia.org/resource/George_Orwell" "http://dbpedia.org/resource/Kurt_Vonnegut" "http://dbpedia.org/resource/John_Updike" "http://dbpedia.org/resource/Jorge_Luis_Borges" "http://dbpedia.org/resource/Julio_Cort%C3%A1zar" "http://dbpedia.org/resource/Naguib_Mahfouz" "http://dbpedia.org/resource/Philip_Jos%C3%A9_Farmer" "http://dbpedia.org/resource/Philip_K._Dick" "http://dbpedia.org/resource/Raymond_Kennedy" "http://dbpedia.org/resource/Richard_McCann" "http://dbpedia.org/resource/Umberto_Eco" "http://dbpedia.org/resource/Vladimir_Nabokov" "http://dbpedia.org/resource/Barun_Roy" "http://dbpedia.org/resource/Teju_Cole" "http://dbpedia.org/resource/Chen_Ran" "http://dbpedia.org/resource/David_Markson" "http://dbpedia.org/resource/Mihail_Sebastian" "http://dbpedia.org/resource/Christopher_Hitchens" "http://dbpedia.org/resource/Yvan_Goll" "http://dbpedia.org/resource/Henry_Miller" "http://dbpedia.org/resource/Gayl_Jones" "http://dbpedia.org/resource/Ignacio_Padilla" "http://dbpedia.org/resource/Joyce_Carol_Oates" "http://dbpedia.org/resource/Mircea_C%C4%83rt%C4%83rescu" "http://dbpedia.org/resource/Carlos_Fuentes" "http://dbpedia.org/resource/Fran_Ross" "http://dbpedia.org/resource/Joseph_Campbell" "http://dbpedia.org/resource/Philip_Roth" "http://dbpedia.org/resource/Guram_Dochanashvili" "http://dbpedia.org/resource/Jack_Kerouac" "http://dbpedia.org/resource/O%C4%9Fuz_Atay" "http://dbpedia.org/resource/Jack_Fritscher" "http://dbpedia.org/resource/Veno_Taufer" "http://dbpedia.org/resource/Prvoslav_Vuj%C4%8Di%C4%87" "http://dbpedia.org/resource/Valya_Dudycz_Lupescu" "http://dbpedia.org/resource/Terence_McKenna" "http://dbpedia.org/resource/Cormac_McCarthy" "http://dbpedia.org/resource/J._G._Ballard" "http://dbpedia.org/resource/John_Dos_Passos" "http://dbpedia.org/resource/Tom_Stoppard" "http://dbpedia.org/resource/Dermot_Healy" "http://dbpedia.org/resource/Dominic_Behan" "http://dbpedia.org/resource/Kevin_McAleer" "http://dbpedia.org/resource/Mo_Yan" "http://dbpedia.org/resource/Jeff_Ragsdale")

;; what did he write?
(get-objects-by-predicate (fetch-dbpedia-rdf "http://dbpedia.org/resource/James_Joyce") :dbpprop:author)

;; ("http://dbpedia.org/resource/Dubliners" "http://dbpedia.org/resource/Stephen_Hero" "http://dbpedia.org/resource/Finnegans_Wake" "http://dbpedia.org/resource/Chamber_Music_(book)" "http://dbpedia.org/resource/Ulysses_(novel)" "http://dbpedia.org/resource/James_Joyce" "http://dbpedia.org/resource/A_Portrait_of_the_Artist_as_a_Young_Man" "http://dbpedia.org/resource/Pomes_Penyeach" "http://dbpedia.org/resource/Giacomo_Joyce" "http://dbpedia.org/resource/A_Mother" "http://dbpedia.org/resource/A_Painful_Case" "http://dbpedia.org/resource/After_the_Race" "http://dbpedia.org/resource/An_Encounter" "http://dbpedia.org/resource/Araby_(short_story)" "http://dbpedia.org/resource/Clay_(short_story)" "http://dbpedia.org/resource/Counterparts_(short_story)" "http://dbpedia.org/resource/Eveline" "http://dbpedia.org/resource/Grace_(short_story)" "http://dbpedia.org/resource/Ivy_Day_in_the_Committee_Room" "http://dbpedia.org/resource/The_Boarding_House" "http://dbpedia.org/resource/The_Dead_(short_story)" "http://dbpedia.org/resource/The_Sisters_(short_story)" "http://dbpedia.org/resource/Two_Gallants_(short_story)" "http://dbpedia.org/resource/Exiles_(play)" "http://dbpedia.org/resource/The_Cats_of_Copenhagen" "http://dbpedia.org/resource/A_Little_Cloud")

;; books written by authors influenced by James Joyce
(def books-by-authors-influenced-by-joyce
  (mapcat #(get-objects-by-predicate (fetch-dbpedia-rdf %) :dbpprop:author)
          (get-objects-by-predicate (fetch-dbpedia-rdf "http://dbpedia.org/resource/James_Joyce") :dbpedia-owl:influencedBy)))

;; what genres are they in?
(->> books-by-authors-influenced-by-joyce
     (map #(-> (enlive/select (fetch-dbpedia-rdf %) [:dbpprop:genre]) first :content first))
     (distinct)
     (remove nil?))

;; this is a mess because the data entered at Wikipedia is a mess, but it's still sort of interesting
("Drama" "Satire, Parody" "Novel" "Postmodernism" "Science fiction and fantasy short stories" "Short story collection" "Fiction/Non-Fiction cross-over" "Modernist/Stream of consciousness" "Non-fiction" "Southern Gothic novel" "I-novel" "Novel, bildungsroman, African-American Literature, social commentary" "General Fiction" "Mock epic; satire" "Colonial literature" "Postmodern novel" "Unknown whether fiction or non-fiction" "Classics, satire, educational animation" "Essays" "Semi-autobiographical novel; Fiction" "Essays, poems and letters" "Science fiction;political fiction" "Short story" "Play" "Tragicomedy, novel" "Travelogue" "Fiction, Horror, Thriller" "Non-Fiction" "Novella, short story collection" "memoir" "Non-fiction anthology" "Novel\n  Page          = 789" "Selections from Novels, Short Stories" "Semi-autobiographical novel" "Semi-Autobiographical novel" "English fiction novel" "Experimental novel or linked stories" "Science fiction" "Science Fiction and fantasy short stories")



;; BTW, I happen to prefer the ntriples format:
;; https://dvcs.w3.org/hg/rdf/raw-file/default/rdf-turtle/n-triples.html

;; JJ ntriples
(def james-n3 (mapv #(clojure.string/split % #"\t") (clojure.string/split-lines (slurp "http://dbpedia.org/data/James_Joyce.n3"))))

(take 10 (drop 721 james-n3))
;; (["dbpedia:Kevin_McAleer" "dbpedia-owl:influencedBy" "dbpedia:James_Joyce ."] ["dbpedia:Mo_Yan" "dbpprop:influences" "dbpedia:James_Joyce ;"] ["" "dbpedia-owl:influencedBy" "dbpedia:James_Joyce ."] ["@prefix ns139:" "<http://wikidata.dbpedia.org/resource/> ."] ["ns139:Q6882" "owl:sameAs" "dbpedia:James_Joyce ."] ["ns97:詹姆斯·乔伊斯" "owl:sameAs" "dbpedia:James_Joyce ."] ["ns115:James_Joyce" "owl:sameAs" "dbpedia:James_Joyce ."] ["ns48:James_Joyce" "owl:sameAs" "dbpedia:James_Joyce ."] ["ns67:جيمس_جويس" "owl:sameAs" "dbpedia:James_Joyce ."] ["ns42:جيمس_جويس" "owl:sameAs" "dbpedia:James_Joyce ."])

(def jj-predicates (distinct (remove #(re-find #" \.$" %) (map second james-n3))))
jj-predicates

(filter #(= "dbpedia-owl:birthDate" (second %)) james-n3)

(map last (filter #(= "dbpprop:influences" (second %)) james-n3))
;;("dbpedia:Donald_Barthelme" "dbpedia:John_Barth" "dbpedia:Bernard_Benstock" "dbpedia:Paul_Auster" "dbpedia:Ronald_Verlin_Cassill" "<http://dbpedia.org/resource/Richard_Yates_(novelist)>" "<http://dbpedia.org/resource/Philip_Jos%C3%A9_Farmer>" "dbpedia:Teju_Cole" "dbpedia:Julien_Green" "<http://dbpedia.org/resource/Julio_Cort%C3%A1zar>" "dbpedia:Dominic_Behan" "dbpedia:Christopher_Hitchens" "dbpedia:Jeff_Ragsdale" "dbpedia:Miho_Mosulishvili" "dbpedia:Veno_Taufer" "<http://dbpedia.org/resource/O%C4%9Fuz_Atay>" "<http://dbpedia.org/resource/Horia_G%C3%A2rbea>" "dbpedia:Mihail_Sebastian" "<http://dbpedia.org/resource/Camilo_Jos%C3%A9_Cela>" "dbpedia:Brendan_Behan" "<http://dbpedia.org/resource/Brian_O\\u0027Nolan>" "dbpedia:Simon_Vestdijk" "dbpedia:Luigi_Malerba" "dbpedia:Malcolm_Lowry" "dbpedia:Anthony_Burgess" "dbpedia:Bonnie_Bluh" "dbpedia:David_Markson" "dbpedia:Yvan_Goll" "dbpedia:Henry_Miller" "dbpedia:Carlos_Fuentes" "dbpedia:Italo_Svevo" "<http://dbpedia.org/resource/J._G._Ballard>" "dbpedia:William_Styron" "dbpedia:Hal_Duncan" "dbpedia:Virginia_Woolf" "dbpedia:Irvine_Welsh" "dbpedia:Alexander_Laurence" "dbpedia:Felix_Aderca" "dbpedia:Mitch_Berman" "dbpedia:George_Orwell" "dbpedia:Vladimir_Nabokov" "dbpedia:Guram_Dochanashvili" "dbpedia:Dylan_Thomas" "dbpedia:Paul_Georgescu" "dbpedia:Chen_Ran" "dbpedia:Gene_Wolfe" "dbpedia:Jerome_Charyn" "<http://dbpedia.org/resource/Samuel_R._Delany>" "dbpedia:Don_DeLillo" "dbpedia:Harold_Bloom" "dbpedia:Ralph_Ellison" "dbpedia:Terence_McKenna" "dbpedia:Gayl_Jones" "dbpedia:Fran_Ross" "<http://dbpedia.org/resource/Philip_K._Dick>" "dbpedia:John_Dos_Passos" "<http://dbpedia.org/resource/Mircea_C%C4%83rt%C4%83rescu>" "dbpedia:Marshall_McLuhan" "dbpedia:Naguib_Mahfouz" "dbpedia:Joseph_Campbell" "<http://dbpedia.org/resource/Lojze_Kova%C4%8Di%C4%8D>" "dbpedia:John_Updike" "dbpedia:Joyce_Carol_Oates" "dbpedia:Saul_Bellow" "dbpedia:William_Faulkner" "dbpedia:William_Michaelian" "dbpedia:Philip_Roth" "dbpedia:Cormac_McCarthy" "dbpedia:Colum_McCann" "dbpedia:Giannina_Braschi" "<http://dbpedia.org/resource/Andr%C3%A9_Aciman>" "<http://dbpedia.org/resource/Edna_O\\u0027Brien>" "<http://dbpedia.org/resource/Jamie_O\\u0027Neill>" "dbpedia:Subimal_Mishra" "dbpedia:Alice_Munro" "dbpedia:Bret_Easton_Ellis" "dbpedia:Martin_Amis" "dbpedia:Richard_McCann" "dbpedia:Umberto_Eco" "dbpedia:Barun_Roy" "dbpedia:Ignacio_Padilla" "dbpedia:Jack_Fritscher" "<http://dbpedia.org/resource/Prvoslav_Vuj%C4%8Di%C4%87>" "dbpedia:Dermot_Healy" "dbpedia:Mo_Yan")



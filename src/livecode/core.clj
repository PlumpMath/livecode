(ns livecode.core
  (:require [net.cgrand.enlive-html :as enlive]
            [clojure.data.json :as json]
            [clj-time.core :as ct]
            [clj-time.format :as cf]))

;; intro
;;; live environment
;;; everything is sequences and functions
;;; repls are cool, but editor-integrated repls are a whole different level

;; data acquisition

;;; copy/paste/clean

;;;; first with emacs, then with clojure version

;; (defn clj-tsv-to-vectors [tsv]
;;   (mapv (fn [line]
;;           (mapv #(let [field (clojure.string/trim %)]
;;                    (if (re-matches #"[\$\.,0-9]+" field) (read-string field) field))
;;                 (clojure.string/split line #"\t"))) (clojure.string/split-lines tsv)))

;; http://en.wikipedia.org/wiki/List_of_countries_by_income_equality
;; (mapv #(vector (first %) (last %)))
;; reduce to country/gini pairs

;; a tiny subset of gini ineqality data
;;(def inequality)

;; http://en.wikipedia.org/wiki/List_of_countries_by_life_expectancy
;; (mapv (fn [[ _ country men women]] (vector country men women)))
;; (reduce (fn [acc [country men women]] (assoc acc country [men women])) {})
;;(def expectancy )

;;;; plotting (use above data)

;;; messy data
;;;; weather (display with quil)

;;; JSON? emacs mode?

;;; XML/semantic web

(defproject livecode "0.1.0-SNAPSHOT"
  :description "A set of tools for live data acquisition and analysis."
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :jvm-opts ["-Xmx4g"]
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [clj-http "0.9.1"]
                 [enlive "1.1.5"]
                 [org.clojure/data.json "0.2.4"]
                 [seabass "2.1.1"]
                 [clj-time "0.4.5"]
                 [incanter "1.5.5"]
                 [quil "2.0.0"]
                 [seesaw "1.4.4"]])

(defproject dale "0.1.0-SNAPSHOT"
  :description "Tool to create file from data"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.cli "0.3.1"]
                 [cuma "0.1.0"]
                 [frontmatter "0.0.1"]
                 [clj-text-decoration "0.0.3"]
                 ]
  :profiles {:dev {;:global-vars {*warn-on-reflection* true}
                   :dependencies [[midje "1.6.3" :exclusions [org.clojure/clojure]]
                                  [org.clojars.runa/conjure "2.1.3"]]}}
  :plugins [[lein-midje "3.1.3"]]
  
  :main dale.core
  :aot [dale.core])

(ns dale.debug
  (:require
    [clojure.pprint :refer [pprint]]
    [text-decoration.core :as deco]))

(def ^:dynamic *color* false)
(def ^:dynamic *debug* false)

(defn debug-log
  [& args]
  (when *debug*
    (doseq [x args]
      ((if (string? x)
         (comp print (if *color* deco/cyan identity))
         pprint) x))
    (print "\n")))

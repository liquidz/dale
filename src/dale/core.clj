(ns dale.core
  (:gen-class)
  (:require
    [clojure.pprint :refer [pprint]]
    [clojure.tools.cli :refer [parse-opts]]
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.edn :as edn]
    [dale.util.file :refer :all]
    [cuma.core :refer [render]]
    [frontmatter.core :as frontmatter]
    [text-decoration.core :as deco]
    )
  (:import [java.io FileNotFoundException]))

(def ^:dynamic *color* false)
(def ^:dynamic *debug* false)

(defn- debug-log
  [& args]
  (when *debug*
    (doseq [x args]
      ((if (string? x)
         (comp print (if *color* deco/cyan identity))
         pprint) x))
    (print "\n")))

(defn read-file
  [path & {:keys [conv default] :or {conv identity, default nil}}]
  (some->> (try
             (slurp path)
             (catch FileNotFoundException e
               default))
           conv))

(defn write-file
  [path]
  (if (sequential? path)
    (doseq [p path] (write-file p))
    (let [{:keys [filename content]} path
          here          (normalize (absolute-path "."))
          abs-path      (normalize (absolute-path filename))
          relative-path (apply str (drop (+ 1 (count here)) abs-path))]
      (some-> relative-path parent mkdirs)
      (spit (io/file relative-path) content))))

(defn- load-data-from-file
  [path]
  (let [ext      (get-last-ext path)
        filename (apply str (drop-last (if ext (+ 1 (count ext)) 0) path))
        res  (case ext
               "edn" (read-file path :conv edn/read-string)
               (frontmatter/parse path))]
    (with-meta res {:filename filename})))

(defn- load-data-from-directory
  [path]
  (->> path
       io/file
       .listFiles
       (filter file?)
       (map (comp load-data-from-file absolute-path))))

(defn load-data
  [data]
  (if (sequential? data)
    (flatten (map load-data data))
    (case (:type data)
      "file" (load-data-from-file (:name data))
      "directory" (load-data-from-directory (:name data))
      nil)))

(defn load-template
  [path]
  ; TODO: template cache
  (read-file path))

(defn- get-filename
  [data]
  (if-let [filename (:filename data)]
    filename
    (-> data meta :filename)))

(defn apply-rule
  [rule]
  (let [data (load-data (:data rule))
        tmpl (load-template (:template rule))
        f    #(let [data* (if (map? %)
                           (with-meta (merge (:default rule {}) %) (meta %))
                           %)
                    filename (get-filename data*)
                    filename (if-let [dir (:output-dir rule)]
                               (join dir filename)
                               filename)
                    ]
                {:filename filename
                 :content  (render tmpl {:data data*})})
        ]

    (debug-log "=== DATA  ===")
    (debug-log data)

    (if (:apply-template-to-each-data rule)
      (map f data)
      (f data))))

(def ^:private cli-options
  [
   [nil  "--color" "Use colors"           :default false]
   [nil  "--debug" "Switch to debug mode" :default false]
   ["-h" "--help"  "Show this help"]])


(defn- usage
  [summary]
  ; TODO: colors
  (let [texts [""
               "USAGE:"
               "  dale (OPTION) rule-file\n"
               "OPTIONS:"
               summary
               ""]]
    (doseq [t texts] (println t))))

(defn -main
  [& args]
  (let [{:keys [options arguments summary errors]} (parse-opts args cli-options)]
    (binding [*debug* (:debug options)
              *color* (:color options)]
      (when errors
        (println "ERROR:")
        (doseq [e errors]
          (println (str "  " e)))
        (usage summary)
        (System/exit 1))

      (when-let [rules (some-> arguments first (read-file :conv edn/read-string))]
        (debug-log "=== RULES ===")
        (debug-log rules)

        ;; TODO; merge default
        (if-not *debug*
          (doseq [r (:rules rules)]
            (->> r (merge {:default (:default rules {})}) apply-rule write-file))
          (doseq [r (:rules rules)]
            (let [res (->> r (merge {:default (:default rules {})}) apply-rule)]
              (debug-log "=== APPLY RESULT ===")
              (debug-log res))))
        ; TODO: error
        )
      )
    )
  )

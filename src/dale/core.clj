(ns dale.core
  (:gen-class)
  (:require
    [clojure.tools.cli :refer [parse-opts]]
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.edn :as edn]
    [dale.util.file :refer :all]
    [cuma.core :refer [render]]
    [frontmatter.core :as frontmatter]
    )
  (:import [java.io FileNotFoundException]))

(def ^:dynamic *debug* false)

(def sample-rule
  {
   :author "uochan"
   :rules [{
            :data {:type "file" :name "foo.edn"}
            :template "test.tpl"
            ; :how-to-apply "each / all at once"
            }]
   }
  )

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
  (let [ext  (get-last-ext path)
        res  (case ext
               "edn" (read-file path :conv edn/read-string)
               (frontmatter/parse path)
               ;{:content (read-file path)}
               )]
    (merge {:filename
            (apply str (drop-last (if ext (+ 1 (count ext)) 0) path))}
           res))
  ;(read-file path :conv edn/read-string)
  )

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
  (if (sequential? data)
    (-> data first get-filename)
    (:filename data)))

(defn apply-rule
  [rule]
  (let [data (load-data (:data rule))
        tmpl (load-template (:template rule))
        f    #(identity {:filename (get-filename %)
                         :content  (render tmpl {:data %})})
        ]

    (if (:apply-template-to-each-data rule)
      (map f data)
      (f data))))

(def ^:private cli-options
  [
   [nil  "--debug" "Switch to debug mode"
    :id      :debug
    :default false]
   ["-h" "--help"  "Show this help"]
   ]
  )

(defn- debug-log
  [& args]
  (when *debug* (apply println args)))

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
    (binding [*debug* (:debug options)]
      ;(debug-log "options  = " options)
      ;(debug-log "argument = " arguments)
      ;(debug-log "summary  = " summary)
      ;(debug-log "errors   = " errors)

      (when errors
        (println "ERROR:")
        (doseq [e errors]
          (println (str "  " e)))
        (usage summary)
        ;TODO: exit
        )

      (when-let [rules (some-> arguments first (read-file :conv edn/read-string))]
        (debug-log "=== RULES ===")
        (debug-log rules "\n")

        ;; TODO; merge default
        (if-not *debug*
          (doseq [r (:rules rules)]
            (->> r (merge (:default rules {})) apply-rule write-file))
          (do
            (debug-log "=== APPLY RESULT ===")
            (doseq [r (:rules rules)]
              (->> r (merge (:default rules {})) apply-rule debug-log))))
        ; TODO: error
        )
      )
    )
  )

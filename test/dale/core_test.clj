(ns dale.core-test
  (:require
    [clojure.string :as str]
    [clojure.java.io :as io]
    [clojure.edn    :as edn]
    [dale.core      :refer :all]
    [dale.util.file :refer [join rm-rf]]
    [midje.sweet    :refer :all]
    [conjure.core   :refer :all]))

(def ^:private test-dir (join "." "test" "files"))

(defn- uniq-name
  [& [ext]] (str (gensym) (if ext (str "." ext))))
(defn- values
  [& ls]
  (let [a (atom ls)]
    (fn [& _]
      (let [[x] @a]
        (swap! a rest)
        x))))

(fact "read-file shoud work fine."
  (let [path (join test-dir "read-file" "test.edn")]
    (str/trim (read-file path)) => "\"test\""
    (read-file path :conv edn/read-string) => "test"
    (read-file "DUMMY") => nil
    (read-file "DUMMY" :default "foo") => "foo"))

(facts "write-file should work fine."
  (fact ""
    (let [path "DUMMY"]
      (write-file {:filename path :content "test"})
      (.exists (io/file path)) => true
      (rm-rf path)))

  (fact "with new directory"
    (let [new-dir (join test-dir "NEWDIR")
          path    (join new-dir "DUMMY")]
      (write-file {:filename path :content "test"})
      (.exists (io/file path)) => true
      (rm-rf new-dir)))

  (fact "write multiple file"
    (let [paths ["DUMMY1" "DUMMY2"]]
      (write-file (map #(identity {:filename % :content "test"}) paths))
      (doseq [p paths]
        (.exists (io/file p)) => true
        (rm-rf p))))
  )

(facts "load-data should work fine."
  (facts "load data from file"
    (fact "load edn file"
      (stubbing [slurp "{:a 1}"]
        (load-data {:type "file" :name (uniq-name "edn")}) => (contains {:a 1})))

    (fact "load json file")
    (fact "load yaml file")
    (fact "load html file")
    (fact "load text file with frontmatter"
      (stubbing [slurp "###\n:foo \"bar\"\n###\nbody"]
        (let [res (load-data {:type "file" :name (uniq-name "txt")})]
          res => (contains {:frontmatter {:foo "bar"}})
          res => (contains {:body "body"})))))

  (fact "load directory"
    (let [path (join test-dir "load-data" "directory")]
      (stubbing [slurp (values "{:a 1}" "{:b 2}")]
        (let [res (load-data {:type "directory" :name path})]
          (first res)  => (contains {:a 1})
          (second res) => (contains {:b 2})))))
  (fact "execute script")

  (fact "load multiple files"
    (stubbing [slurp (values "{:a 1}" "{:b 2}")]
      (let [res (load-data [{:type "file" :name (uniq-name "edn")}
                            {:type "file" :name (uniq-name "edn")}])]
        (first res)  => (contains {:a 1})
        (second res) => (contains {:b 2}))))

  (fact "load mixed data"
    (let [path (join test-dir "load-data" "directory")]
      (stubbing [slurp (values "{:a 1}" "{:b 2}" "{:c 3}")]
        (let [res (load-data [{:type "file" :name (uniq-name "edn")}
                    {:type "directory" :name path}])]
          (nth res 0) => (contains {:a 1})
          (nth res 1) => (contains {:b 2})
          (nth res 2) => (contains {:c 3}))))))


(facts "apply-rule should work fine."
  (fact "single data"
    (stubbing [load-data     {:a 123}
               load-template "a = $(data.a)"]
      (apply-rule {:data "STUB"
                   :template "STUB"}) => (contains {:content "a = 123"})))

  (fact "multiple data"
    (stubbing [load-data     [{:a 123} {:a 456}]
               load-template "@(for data)a = $(a)@(end)"]
      (apply-rule {:data ["STUB" "STUB"]
                   :template "STUB"}) => (contains {:content "a = 123a = 456"})))

  (fact "aplly each data to template"
    (stubbing [load-data [{:a 123} {:a 456}]
               load-template "a = $(data.a)"]
      (let [res (apply-rule {:data ["STUB" "STUB"]
                             :template "STUB"
                             :apply-template-to-each-data true})]
        (first res)  => (contains {:content "a = 123"})
        (second res) => (contains {:content "a = 456"}))))

  (fact "implicit filename"
    (stubbing [slurp         "{:a 1}"
               load-template "a = $(data.a)"]
      (let [name (uniq-name "foo")
            name* (str name ".edn")]
        (apply-rule {:data {:type "file" :name name*}
                     :template "STUB"}) => (contains {:filename name}))))

  (fact "explicit filename"
    (stubbing [slurp "{:filename \"foo.txt\" :a 1}"
               load-template "a = $(data.a)"]
      (apply-rule {:data {:type "file" :name (uniq-name "edn")}
                   :template "STUB"}) => (contains {:filename "foo.txt"})))

  (fact "output directory"
    (stubbing [load-data {:filename "bar.txt" :a 123}
               load-template "a = $(a)"]
      (apply-rule {:data "STUB"
                   :template "STUB"
                   :output-dir "foo"}) => (contains {:filename "foo/bar.txt"})))

  (fact "default value"
    (stubbing [load-data {:a 123}
               load-template "a = $(data.a), b = $(data.b)"]
      (apply-rule {:data "STUB"
                   :template "STUB"
                   :default {:b 456}}) => (contains {:content "a = 123, b = 456"})))
  )


(facts "run should work fine."
  (let [res (atom nil)
        wf  #(reset! res %)]
    (fact "normal usage"
      (stubbing [load-data {:a 123}
                 load-template "a = $(data.a)"
                 write-file wf]
        (run {:rules [{:data "STUB" :template "STUB"}]})
        (:content @res) => "a = 123"))

    (fact "global default"
      (stubbing [load-data {:a 123}
                 load-template "a = $(data.a), b = $(data.b)"
                 write-file wf]
        (run {:default {:b 345} :rules [{:data "STUB" :template "STUB"}]})
        (:content @res) => "a = 123, b = 345"))

    (fact "global default and local default"
      (stubbing [load-data {:a 123}
                 load-template "a = $(data.a), b = $(data.b), c = $(data.c)"
                 write-file wf]
        (run {:default {:b 345} :rules [{:data "STUB" :template "STUB" :default {:c 678}}]})
        (:content @res) => "a = 123, b = 345, c = 678"))))

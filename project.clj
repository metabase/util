(defproject metabase/util "1.0.0"
  :min-lein-version "2.5.0"

  :aliases
  {"test"                      ["with-profile" "+expectations" "expectations"]
   "bikeshed"                  ["bikeshed" "--max-line-length" "150"]
   "check-namespace-decls"     ["with-profile" "+check-namespace-decls" "check-namespace-decls"]
   "docstring-checker"         ["with-profile" "+docstring-checker" "docstring-checker"]
   "eastwood"                  ["with-profile" "+eastwood" "eastwood"]
   "check-reflection-warnings" ["with-profile" "+reflection-warnings" "check"]
   ;; `lein lint` will run all linters
   "lint"                      ["do" ["eastwood"] ["bikeshed"] ["check-namespace-decls"] ["docstring-checker"]]}

  :dependencies
  [[org.clojure/clojure "1.10.0"]
   [org.clojure/java.classpath "0.3.0"]
   [org.clojure/math.numeric-tower "0.0.4"]
   [org.clojure/tools.logging "0.4.1"]
   [org.clojure/tools.namespace "0.2.11"]
   [cheshire "5.8.1"]
   [clj-time "0.15.1"]
   [colorize "0.1.1" :exclusions [org.clojure/clojure]]
   [com.jcraft/jsch "0.1.55"]
   [environ "1.1.0"]
   [honeysql "0.9.4" :exclusions [org.clojure/clojurescript]]
   [javax.xml.bind/jaxb-api "2.4.0-b180830.0359"]
   [medley "1.0.0"]
   [prismatic/schema "1.1.9"]
   [puppetlabs/i18n "0.8.0"]
   [ring/ring-codec "1.1.1"]]

  :profiles
  {:dev
   {:dependencies
    [[expectations "2.2.0-beta2"]]

    :plugins
    [[docstring-checker "1.0.3"]
     [jonase/eastwood "0.3.1"
      :exclusions [org.clojure/clojure]]
     [lein-bikeshed "0.4.1"]
     [lein-check-namespace-decls "1.0.1"]
     [lein-environ "1.1.0"]
     [lein-expectations "0.0.8"]]

    :injections
    [(require 'expectations)
     (expectations/disable-run-on-shutdown)]}

   :eastwood
   {:eastwood
    {:add-linters
     [:unused-private-vars
      :unused-namespaces
      :unused-fn-args
      :unused-locals]}}

   :docstring-checker
   {:docstring-checker
    {:exclude [#"test"]}}

   :reflection-warnings
   {:global-vars {*warn-on-reflection* true}}

   :check-namespace-decls
   {:source-paths ["test"]}})

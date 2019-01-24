(defproject metabase/util "1.0.0"

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
   [ring/ring-codec "1.1.1"]])

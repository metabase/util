(ns metabase.util.config
  (:refer-clojure :exclude [boolean keyword])
  (:require [clojure
             [core :as core]
             [string :as str]]
            [environ.core :as env])
  (:import clojure.lang.Keyword))

;;; ---------------------------------------------------- sources -----------------------------------------------------

(defmulti get-value-from-source
  "Attempt to resolve the value of a particular configuration setting named key `k` from a particular `source`. If the
  source does not value for that value, return `not-found`. Add new implementations of this method to let Metabase use
  new configuration sources.

    (defmethod get-value-from-source :config-file [_ k not-found]
      (or (fetch-value-from-config-file v)
          not-found))"
  {:arglists '([source k not-found])}
  (fn [source _ _]
    source))

(defmethod get-value-from-source :env [_ k not-found]
  (env/env k not-found))

(defonce
  ^{:doc "All config sources that should be used to resolve configuration values, in order they should be tried.

  When attempting to resolve a configuration value, looks thru each source (i.e., calls `get-value-from-source` with
  each value here as the dispatch value.) each value listed here as the first arg).

  To add a new configuration source, add an implementation of `get-value-from-source` and add the dispatch value to
  the list."}
  sources
  (atom [:env]))


(defmethod get-value-from-source ::configured-sources [_ k not-found]
  (let [k (core/keyword k)]
    (loop [[source & more] @sources]
      (let [v (get-value-from-source source k not-found)]
        (cond
          (not= v not-found) v
          (seq more)         (recur more)
          :else              not-found)))))


;;; ------------------------------------------------- getting values -------------------------------------------------

(defn string
  "Fetch a configuration value and return it as a String."
  ([k]
   (string k nil))

  ([k not-found]
   (str (get-value-from-source ::configured-sources k not-found))))


(defn- string-parsed-by
  ([parse-fn k]
   (string-parsed-by parse-fn k nil))

  ([parse-fn k not-found]
   (let [v (string k not-found)]
     (cond
       (and (string? v)
            (not= v not-found)) (parse-fn v)
       (not= v not-found)       v
       :else                    not-found))))

;; These methods only parse configuration values if the come back from their sources as a STRING (e.g. env vars).
;; Other sources (such as JSON or YAML configuration files) that support non-string types can return those values
;; as-is; the parsing functions will be skipped.

(def ^{:arglists '([k] [k not-found])} ^Boolean boolean
  "Fetch a configuration value and parse it as a Boolean."
  (partial string-parsed-by core/boolean))

(def ^{:arglists '([k] [k not-found])} ^Integer integer
  "Fetch a configuration value and parse it as an Integer."
  (partial string-parsed-by #(Integer/parseInt %)))

(def ^{:arglists '([k] [k not-found])} ^Keyword keyword
  "Fetch a configuration value and parse it as a Keyword."
  (partial string-parsed-by core/keyword))

;;; ------------------------------------------------------ Util ------------------------------------------------------

(def ^Boolean is-windows?
  "Are we running on a Windows machine?"
  (str/includes? (str/lower-case (string :os-name)) "win"))

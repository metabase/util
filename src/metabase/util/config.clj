(ns metabase.util.config
  (:require [clojure.string :as str]
            [environ.core :as env]))

;;; ---------------------------------------------------- sources -----------------------------------------------------

(defmulti get-value-from-source
  ""
  {:arglists '([source k not-found])}
  (fn [source _ _]
    source))

(defmethod get-value-from-source :env [_ k not-found]
  (env/env k not-found))

(defonce sources (atom [:env]))

;;; ------------------------------------------------- getting values -------------------------------------------------

(defn get-string
  ([k]
   (get-string k nil))
  ([k not-found]
   (let [k (keyword k)]
     (loop [[source & more] @sources]
       (let [v (get-value-from-source source k not-found)]
         (cond
           (not= v not-found)
           v

           (seq more)
           (recur more)

           :else
           not-found))))))

(defn- get-string-and-parse
  ([parse-fn k]
   (get-string-and-parse parse-fn k nil))
  ([parse-fn k not-found]
   (if-let [v (get-string k)]
     (parse-fn v)
     not-found)))

(def ^{:arglists '([k] [k not-found])} get-boolean
  (partial get-string-and-parse #(Boolean/parseBoolean %)))

(def ^{:arglists '([k] [k not-found])} get-integer
  (partial get-string-and-parse #(Integer/parseInt %)))

(def ^{:arglists '([k] [k not-found])} get-keyword
  (partial get-string-and-parse keyword))

;;; ------------------------------------------------------ Util ------------------------------------------------------

(def ^Boolean is-windows?
  "Are we running on a Windows machine?"
  (str/includes? (str/lower-case (System/getProperty "os.name")) "win"))

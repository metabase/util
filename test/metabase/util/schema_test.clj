(ns metabase.util.schema-test
  "Tests for utility schemas and various API helper functions."
  (:require [expectations :refer [expect]]
            [metabase.util.schema :as su]
            [puppetlabs.i18n.core :as i18n]
            [schema.core :as s]))

;; Needed for side-effects (?)
(require 'metabase.util.i18n)

;; check that the API error message generation is working as intended
(expect
  (str "value may be nil, or if non-nil, value must satisfy one of the following requirements: "
       "1) value must be a boolean. "
       "2) value must be a valid boolean string ('true' or 'false').")
  (str (su/api-error-message (s/maybe (s/cond-pre s/Bool su/BooleanString)))))

(defn- ex-info-msg [f]
  (try
    (f)
    (catch clojure.lang.ExceptionInfo e
      (.getMessage e))))

(expect
  #"Integer greater than zero"
  (let [zz (i18n/string-as-locale "zz")]
    (i18n/with-user-locale zz
      (ex-info-msg #(s/validate su/IntGreaterThanZero -1)))))

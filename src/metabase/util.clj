(ns metabase.util
  "Common utility functions useful throughout the codebase."
  (:require [clojure
             [data :as data]
             [pprint :refer [pprint]]
             [string :as str]
             [walk :as walk]]
            [clojure.java.classpath :as classpath]
            [clojure.math.numeric-tower :as math]
            [clojure.tools.logging :as log]
            [clojure.tools.namespace.find :as ns-find]
            [colorize.core :as colorize]
            [medley.core :as m]
            [metabase.util
             [config :as config.u]
             [i18n :refer [tru]]]
            [ring.util.codec :as codec])
  (:import [java.text Normalizer Normalizer$Form]
           java.util.concurrent.TimeoutException
           javax.xml.bind.DatatypeConverter))

(defn format-bytes
  "Nicely format `num-bytes` as kilobytes/megabytes/etc.

    (format-bytes 1024) ; -> 2.0 KB"
  [num-bytes]
  (loop [n num-bytes [suffix & more] ["B" "KB" "MB" "GB"]]
    (if (and (seq more)
             (>= n 1024))
      (recur (/ n 1024.0) more)
      (format "%.1f %s" n suffix))))

(defmacro ignore-exceptions
  "Simple macro which wraps the given expression in a try/catch block and ignores the exception if caught."
  {:style/indent 0}
  [& body]
  `(try ~@body (catch Throwable ~'_)))

;;; ## Etc

(defn optional
  "Helper function for defining functions that accept optional arguments. If PRED? is true of the first item in ARGS,
  a pair like `[first-arg other-args]` is returned; otherwise, a pair like `[DEFAULT other-args]` is returned.

   If DEFAULT is not specified, `nil` will be returned when PRED? is false.

    (defn
      ^{:arglists ([key? numbers])}
      wrap-nums [& args]
      (let [[k nums] (optional keyword? args :nums)]
        {k nums}))
    (wrap-nums 1 2 3)          -> {:nums [1 2 3]}
    (wrap-nums :numbers 1 2 3) -> {:numbers [1 2 3]}"
  {:arglists '([pred? args]
               [pred? args default])}
  [pred? args & [default]]
  (if (pred? (first args)) [(first args) (next args)]
      [default args]))


(defn email?
  "Is STRING a valid email address?"
  ^Boolean [^String s]
  (boolean (when (string? s)
             (re-matches #"[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?"
                         (str/lower-case s)))))


(defn url?
  "Is STRING a valid HTTP/HTTPS URL? (This only handles `localhost` and domains like `metabase.com`; URLs containing
  IP addresses will return `false`.)"
  ^Boolean [^String s]
  (boolean (when (seq s)
             (when-let [^java.net.URL url (ignore-exceptions (java.net.URL. s))]
               ;; these are both automatically downcased
               (let [protocol (.getProtocol url)
                     host     (.getHost url)]
                 (and protocol
                      host
                      (re-matches #"^https?$" protocol)
                      (or (re-matches #"^.+\..{2,}$" host) ; 2+ letter TLD
                          (= host "localhost"))))))))

(defn sequence-of-maps?
  "Is COLL a sequence of maps?"
  [coll]
  (and (sequential? coll)
       (every? map? coll)))

(defn maybe?
  "Returns `true` if X is `nil`, otherwise calls (F X).
   This can be used to see something is either `nil` or statisfies a predicate function:

     (string? nil)          -> false
     (string? \"A\")        -> true
     (maybe? string? nil)   -> true
     (maybe? string? \"A\") -> true

   It can also be used to make sure a given function won't throw a `NullPointerException`:

     (str/lower-case nil)            -> NullPointerException
     (str/lower-case \"ABC\")        -> \"abc\"
     (maybe? str/lower-case nil)     -> true
     (maybe? str/lower-case \"ABC\") -> \"abc\"

   The latter use-case can be useful for things like sorting where some values in a collection
   might be `nil`:

     (sort-by (partial maybe? str/lower-case) some-collection)"
  [f x]
  (or (nil? x)
      (f x)))

(defn rpartial
  "Like `partial`, but applies additional args *before* BOUND-ARGS.
   Inspired by [`-rpartial` from dash.el](https://github.com/magnars/dash.el#-rpartial-fn-rest-args)

    ((partial - 5) 8)  -> (- 5 8) -> -3
    ((rpartial - 5) 8) -> (- 8 5) -> 3"
  [f & bound-args]
  (fn [& args]
    (apply f (concat args bound-args))))

(defmacro prog1
  "Execute FIRST-FORM, then any other expressions in BODY, presumably for side-effects; return the result of
   FIRST-FORM.

     (def numbers (atom []))

     (defn find-or-add [n]
       (or (first-index-satisfying (partial = n) @numbers)
           (prog1 (count @numbers)
             (swap! numbers conj n))))

     (find-or-add 100) -> 0
     (find-or-add 200) -> 1
     (find-or-add 100) -> 0

   The result of FIRST-FORM is bound to the anaphor `<>`, which is convenient for logging:

     (prog1 (some-expression)
       (println \"RESULTS:\" <>))

  `prog1` is an anaphoric version of the traditional macro of the same name in
   [Emacs Lisp](http://www.gnu.org/software/emacs/manual/html_node/elisp/Sequencing.html#index-prog1)
   and [Common Lisp](http://www.lispworks.com/documentation/HyperSpec/Body/m_prog1c.htm#prog1).

  Style note: Prefer `doto` when appropriate, e.g. when dealing with Java objects."
  {:style/indent 1}
  [first-form & body]
  `(let [~'<> ~first-form]
     ~@body
     ~'<>))

(def ^:private emoji*
  (delay
   (if (config.u/get-boolean :mb-emoji-in-logs)
     identity
     (constantly ""))))

(defn emoji
  "Returns the `emoji-string` passed in if emoji in logs are enabled, otherwise always returns an empty string."
  ^String [emoji-string]
  (@emoji* emoji-string))

(def ^:private colorize*
  (delay
   (if (config.u/get-boolean :mb-colorize-logs)
     (fn [color x]
       (colorize/color (keyword color) x))
     (fn [_ x]
       x))))

(defn- colorize
  "Colorize string `x` with the function matching `color` symbol or keyword, but only if `MB_COLORIZE_LOGS` is
  enabled (the default)."
  [color-symb x]
  (@colorize* color-symb x))

(defn format-color
  "Like `format`, but colorizes the output. `color` should be a symbol or keyword like `green`, `red`, `yellow`, `blue`,
  `cyan`, `magenta`, etc. See the entire list of avaliable
  colors [here](https://github.com/ibdknox/colorize/blob/master/src/colorize/core.clj).

     (format-color :red \"Fatal error: %s\" error-message)"
  {:style/indent 2}
  (^String [color x]
   {:pre [((some-fn symbol? keyword?) color)]}
   (colorize color (str x)))

  (^String [color format-string & args]
   (colorize color (apply format (str format-string) args))))

(defn pprint-to-str
  "Returns the output of pretty-printing `x` as a string.
  Optionally accepts `color-symb`, which colorizes the output with the corresponding
  function from `colorize.core`.

     (pprint-to-str 'green some-obj)"
  {:style/indent 1}
  (^String [x]
   (when x
     (with-out-str (pprint x))))
  (^String [color-symb x]
   (colorize color-symb (pprint-to-str x))))


(defprotocol ^:private IFilteredStacktrace
  (filtered-stacktrace [this]
    "Get the stack trace associated with E and return it as a vector with non-metabase frames after the last Metabase
    frame filtered out."))

;; These next two functions are a workaround for this bug https://dev.clojure.org/jira/browse/CLJ-1790
;; When Throwable/Thread are type-hinted, they return an array of type StackTraceElement, this causes
;; a VerifyError. Adding a layer of indirection here avoids the problem. Once we upgrade to Clojure 1.9
;; we should be able to remove this code.
(defn- throwable-get-stack-trace [^Throwable t]
  (.getStackTrace t))

(defn- thread-get-stack-trace [^Thread t]
  (.getStackTrace t))

(extend nil
  IFilteredStacktrace {:filtered-stacktrace (constantly nil)})

(extend Throwable
  IFilteredStacktrace {:filtered-stacktrace (fn [this]
                                             (filtered-stacktrace (throwable-get-stack-trace this)))})

(extend Thread
  IFilteredStacktrace {:filtered-stacktrace (fn [this]
                                              (filtered-stacktrace (thread-get-stack-trace this)))})

(defn- metabase-frame? [frame]
  (re-find #"metabase" (str frame)))

;; StackTraceElement[] is what the `.getStackTrace` method for Thread and Throwable returns
(extend (Class/forName "[Ljava.lang.StackTraceElement;")
  IFilteredStacktrace
  {:filtered-stacktrace
   (fn [this]
     ;; keep all the frames before the last Metabase frame, but then filter out any other non-Metabase frames after
     ;; that
     (let [[frames-after-last-mb other-frames]     (split-with (complement metabase-frame?)
                                                               (map str (seq this)))
           [last-mb-frame & frames-before-last-mb] (map #(str/replace % #"^metabase\." "")
                                                        (filter metabase-frame? other-frames))]
       (concat
        frames-after-last-mb
        ;; add a little arrow to the frame so it stands out more
        (cons (str "--> " last-mb-frame)
              frames-before-last-mb))))})

(defn deref-with-timeout
  "Call `deref` on a FUTURE and throw an exception if it takes more than TIMEOUT-MS."
  [futur timeout-ms]
  (let [result (deref futur timeout-ms ::timeout)]
    (when (= result ::timeout)
      (throw (TimeoutException. (format "Timed out after %d milliseconds." timeout-ms))))
    result))

(defmacro with-timeout
  "Run BODY in a `future` and throw an exception if it fails to complete after TIMEOUT-MS."
  [timeout-ms & body]
  `(deref-with-timeout (future ~@body) ~timeout-ms))

(defn round-to-decimals
  "Round (presumabily floating-point) NUMBER to DECIMAL-PLACE. Returns a `Double`.

     (round-to-decimals 2 35.5058998M) -> 35.51"
  ^Double [^Integer decimal-place, ^Number number]
  {:pre [(integer? decimal-place) (number? number)]}
  (double (.setScale (bigdec number) decimal-place BigDecimal/ROUND_HALF_UP)))

(defn ^:deprecated drop-first-arg
  "Returns a new fn that drops its first arg and applies the rest to the original.
   Useful for creating `extend` method maps when you don't care about the `this` param. :flushed:

     ((drop-first-arg :value) xyz {:value 100}) -> (apply :value [{:value 100}]) -> 100"
  ^clojure.lang.IFn [^clojure.lang.IFn f]
  (comp (partial apply f) rest list))


(defn- check-protocol-impl-method-map
  "Check that the methods expected for PROTOCOL are all implemented by METHOD-MAP, and that no extra methods are
   provided. Used internally by `strict-extend`."
  [protocol method-map]
  (let [[missing-methods extra-methods] (data/diff (set (keys (:method-map protocol))) (set (keys method-map)))]
    (when missing-methods
      (throw (Exception. (format "Missing implementations for methods in %s: %s" (:var protocol) missing-methods))))
    (when extra-methods
      (throw (Exception. (format "Methods implemented that are not in %s: %s " (:var protocol) extra-methods))))))

(defn strict-extend
  "A strict version of `extend` that throws an exception if any methods declared in the protocol are missing or any
  methods not declared in the protocol are provided.

  Since this has better compile-time error-checking, prefer `strict-extend` to regular `extend` in all situations, and
  to `extend-protocol`/ `extend-type` going forward."
  ;; TODO - maybe implement strict-extend-protocol and strict-extend-type ?
  {:style/indent 1}
  [atype protocol method-map & more]
  (check-protocol-impl-method-map protocol method-map)
  (extend atype protocol method-map)
  (when (seq more)
    (apply strict-extend atype more)))

(defn remove-diacritical-marks
  "Return a version of S with diacritical marks removed."
  ^String [^String s]
  (when (seq s)
    (str/replace
     ;; First, "decompose" the characters. e.g. replace 'LATIN CAPITAL LETTER A WITH ACUTE' with 'LATIN CAPITAL LETTER
     ;; A' + 'COMBINING ACUTE ACCENT' See http://docs.oracle.com/javase/8/docs/api/java/text/Normalizer.html
     (Normalizer/normalize s Normalizer$Form/NFD)
     ;; next, remove the combining diacritical marks -- this SO answer explains what's going on here best:
     ;; http://stackoverflow.com/a/5697575/1198455 The closest thing to a relevant JavaDoc I could find was
     ;; http://docs.oracle.com/javase/7/docs/api/java/lang/Character.UnicodeBlock.html#COMBINING_DIACRITICAL_MARKS
     #"\p{Block=CombiningDiacriticalMarks}+"
     "")))


(def ^:private ^:const slugify-valid-chars
  "Valid *ASCII* characters for URL slugs generated by `slugify`."
  #{\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z
    \0 \1 \2 \3 \4 \5 \6 \7 \8 \9
    \_})

;; unfortunately it seems that this doesn't fully-support Emoji :(, they get encoded as "??"
(defn- slugify-char [^Character c]
  (cond
    (> (int c) 128)                   (codec/url-encode c) ; for non-ASCII characters, URL-encode them
    (contains? slugify-valid-chars c) c                    ; for ASCII characters, if they're in the allowed set of characters, keep them
    :else                             \_))                 ; otherwise replace them with underscores

(defn slugify
  "Return a version of `String` S appropriate for use as a URL slug.
   Downcase the name, remove diacritcal marks, and replace non-alphanumeric *ASCII* characters with underscores;
   URL-encode non-ASCII characters. (Non-ASCII characters are encoded rather than replaced with underscores in order
   to support languages that don't use the Latin alphabet; see issue #3818).

   Optionally specify MAX-LENGTH which will truncate the slug after that many characters."
  (^String [^String s]
   (when (seq s)
     (str/join (for [c (remove-diacritical-marks (str/lower-case s))]
               (slugify-char c)))))
  (^String [s max-length]
   (str/join (take max-length (slugify s)))))

(defn do-with-auto-retries
  "Execute F, a function that takes no arguments, and return the results.
   If F fails with an exception, retry F up to NUM-RETRIES times until it succeeds.

   Consider using the `auto-retry` macro instead of calling this function directly."
  {:style/indent 1}
  [num-retries f]
  (if (<= num-retries 0)
    (f)
    (try (f)
         (catch Throwable e
           (log/warn (format-color 'red "auto-retry %s: %s" f (.getMessage e)))
           (do-with-auto-retries (dec num-retries) f)))))

(defmacro auto-retry
  "Execute BODY and return the results.
   If BODY fails with an exception, retry execution up to NUM-RETRIES times until it succeeds."
  {:style/indent 1}
  [num-retries & body]
  `(do-with-auto-retries ~num-retries
     (fn [] ~@body)))

(defn key-by
  "Convert a sequential COLL to a map of `(f item)` -> `item`.
  This is similar to `group-by`, but the resultant map's values are single items from COLL rather than sequences of
  items. (Because only a single item is kept for each value of `f`, items producing duplicate values will be
  discarded).

     (key-by :id [{:id 1, :name :a} {:id 2, :name :b}]) -> {1 {:id 1, :name :a}, 2 {:id 2, :name :b}}"
  {:style/indent 1}
  [f coll]
  (into {} (map (juxt f identity)) coll))

(defn keyword->qualified-name
  "Return keyword K as a string, including its namespace, if any (unlike `name`).

     (keyword->qualified-name :type/FK) ->  \"type/FK\""
  [k]
  (when k
    (str/replace (str k) #"^:" "")))

(defn id
  "If passed an integer ID, returns it. If passed a map containing an `:id` key, returns the value if it is an integer.
  Otherwise returns `nil`.

  Provided as a convenience to allow model-layer functions to easily accept either an object or raw ID. Use this in
  cases where the ID/object is allowed to be `nil`. Use `get-id` below in cases where you would also like to guarantee
  it is non-`nil`."
  ^Integer [object-or-id]
  (cond
    (map? object-or-id)     (recur (:id object-or-id))
    (integer? object-or-id) object-or-id))

;; TODO - now that I think about this, I think this should be called `the-id` instead, because the idea is similar to
;; `clojure.core/the-ns`
(defn get-id
  "If passed an integer ID, returns it. If passed a map containing an `:id` key, returns the value if it is an integer.
  Otherwise, throws an Exception.

  Provided as a convenience to allow model-layer functions to easily accept either an object or raw ID, and to assert
  that you have a valid ID."
  ;; TODO - lots of functions can be rewritten to use this, which would make them more flexible
  ^Integer [object-or-id]
  (or (id object-or-id)
      (throw (Exception. (str (tru "Not something with an ID: {0}" object-or-id))))))

(def metabase-namespace-symbols
  "Delay to a vector of symbols of all Metabase namespaces, excluding test namespaces.
   This is intended for use by various routines that load related namespaces, such as task and events initialization.
   Using `ns-find/find-namespaces` is fairly slow, and can take as much as half a second to iterate over the thousand
   or so namespaces that are part of the Metabase project; use this instead for a massive performance increase."
  ;; We want to give JARs in the ./plugins directory a chance to load. At one point we have this as a future so it
  ;; start looking for things in the background while other stuff is happening but that meant plugins couldn't
  ;; introduce new Metabase namespaces such as drivers.
  (delay (vec (for [ns-symb (ns-find/find-namespaces (classpath/system-classpath))
                    :when   (and (.startsWith (name ns-symb) "metabase.")
                                 (not (.contains (name ns-symb) "test")))]
                ns-symb))))

(def ^java.util.regex.Pattern uuid-regex
  "A regular expression for matching canonical string representations of UUIDs."
  #"[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}")


(defn select-nested-keys
  "Like `select-keys`, but can also handle nested keypaths:

     (select-nested-keys {:a 100, :b {:c 200, :d 300}} [:a [:b :d] :c])
     ;; -> {:a 100, :b {:d 300}}

   The values of KEYSEQ can be either regular keys, which work the same way as `select-keys`,
   or vectors of the form `[k & nested-keys]`, which call `select-nested-keys` recursively
   on the value of `k`. "
  [m keyseq]
  ;; TODO - use (empty m) once supported by model instances
  (into {} (for [k     keyseq
                 :let  [[k & nested-keys] (if (sequential? k) k [k])
                        v                 (get m k)]
                 :when (contains? m k)]
             {k (if-not (seq nested-keys)
                  v
                  (select-nested-keys v nested-keys))})))

(defn base64-string?
  "Is `s` a Base-64 encoded string?"
  ^Boolean [s]
  (boolean (when (string? s)
             (re-find #"^[0-9A-Za-z/+]+=*$" s))))

(defn decode-base64
  "Decodes a Base64 string to a UTF-8 string"
  [input]
  (new java.lang.String (DatatypeConverter/parseBase64Binary input) "UTF-8"))

(defn encode-base64
  "Encodes a string to a Base64 string"
  [^String input]
  (DatatypeConverter/printBase64Binary (.getBytes input "UTF-8")))

(def ^{:arglists '([n])} safe-inc
  "Increment N if it is non-`nil`, otherwise return `1` (e.g. as if incrementing `0`)."
  (fnil inc 0))

(defn occurances-of-substring
  "Return the number of times SUBSTR occurs in string S."
  ^Long [^String s, ^String substr]
  (when (and (seq s) (seq substr))
    (loop [index 0, cnt 0]
      (if-let [^long new-index (str/index-of s substr index)]
        (recur (inc new-index) (inc cnt))
        cnt))))

(defn select-non-nil-keys
  "Like `select-keys`, but returns a map only containing keys in KS that are present *and non-nil* in M.

     (select-non-nil-keys {:a 100, :b nil} #{:a :b :c})
     ;; -> {:a 100}"
  [m ks]
  (into {} (for [k     ks
                 :when (some? (get m k))]
             {k (get m k)})))

(defn select-keys-when
  "Returns a map that only contains keys that are either `:present` or `:non-nil`. Combines behavior of `select-keys`
  and `select-non-nil-keys`. This is useful for API endpoints that update a model, which often have complex rules
  about what gets updated (some keys are updated if `nil`, others only if non-nil).

     (select-keys-when {:a 100, :b nil, :d 200, :e nil}
       :present #{:a :b :c}
       :non-nil #{:d :e :f})
     ;; -> {:a 100, :b nil, :d 200}"
  {:style/indent 1}
  [m & {:keys [present non-nil]}]
  (merge (select-keys m present)
         (select-non-nil-keys m non-nil)))

(defn order-of-magnitude
  "Return the order of magnitude as a power of 10 of a given number."
  [x]
  (if (zero? x)
    0
    (long (math/floor (/ (Math/log (math/abs x))
                         (Math/log 10))))))

(defn update-when
  "Like clojure.core/update but does not create a new key if it does not exist.
   Useful when you don't want to create cruft."
  [m k f & args]
  (if (contains? m k)
    (apply update m k f args)
    m))

(defn update-in-when
  "Like clojure.core/update-in but does not create new keys if they do not exist.
   Useful when you don't want to create cruft."
  [m k f & args]
  (if (not= ::not-found (get-in m k ::not-found))
    (apply update-in m k f args)
    m))

(defn index-of
  "Return index of the first element in `coll` for which `pred` reutrns true."
  [pred coll]
  (first (keep-indexed (fn [i x]
                         (when (pred x) i))
                       coll)))


(defn is-java-9-or-higher?
  "Are we running on Java 9 or above?"
  ([]
   (is-java-9-or-higher? (System/getProperty "java.version")))
  ([java-version-str]
   (when-let [[_ java-major-version-str] (re-matches #"^(?:1\.)?(\d+).*$" java-version-str)]
     (>= (Integer/parseInt java-major-version-str) 9))))

(defn hexadecimal-string?
  "Returns truthy if `new-value` is a hexadecimal-string"
  [new-value]
  (and (string? new-value)
       (re-matches #"[0-9a-f]{64}" new-value)))

(defn snake-key
  "Convert a keyword or string `k` from `lisp-case` to `snake-case`."
  [k]
  (if (keyword? k)
    (keyword (snake-key (name k)))
    (str/replace k #"-" "_")))

(defn recursive-map-keys
  "Recursively replace the keys in a map with the value of `(f key)`."
  [f m]
  (walk/postwalk
   #(if (map? %)
      (m/map-keys f %)
      %)
   m))

(defn snake-keys
  "Convert the keys in a map from `lisp-case` to `snake-case`."
  [m]
  (recursive-map-keys snake-key m))

(defn one-or-many
  "Wraps a single element in a sequence; returns sequences as-is. In lots of situations we'd like to accept either a
  single value or a collection of values as an argument to a function, and then loop over them; rather than repeat
  logic to check whether something is a collection and wrap if not everywhere, this utility function is provided for
  your convenience.

    (u/one-or-many 1)     ; -> [1]
    (u/one-or-many [1 2]) ; -> [1 2]"
  [arg]
  (if ((some-fn sequential? set?) arg)
    arg
    [arg]))

(defmacro varargs
  "Make a properly-tagged Java interop varargs argument."
  [klass & [objects]]
  (vary-meta `(into-array ~klass ~objects)
             assoc :tag (format "[L%s;" (.getCanonicalName ^Class (ns-resolve *ns* klass)))))

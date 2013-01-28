(ns pallet.stevedore.common
  (:require
   [clojure.java.io :refer [file]]
   [clojure.string :as string]
   [clojure.tools.logging :refer [tracef]]
   [pallet.stevedore
    :refer [chain-commands checked-commands do-script emit empty-splice
            filter-empty-splice *script-fn-dispatch* *script-language*
            with-script-language]]))


;; Main dispatch functions.
;;
;; `stevedore/emit` is the entry point for parsing.
;; It dispatches on the type of its argument.
;;
;; For example (emit (ls "asdf")) dispatches on `clojure.lang.IPersistentList`.
;;
;; The above example, along with some others, call `emit-special`
;; `emit-function` or `emit-function-call`.
;;
;; For example:
;;
;;  (emit (ls "asdf"))
;; calls
;;  (emit-special 'ls (ls "asdf"))
;;
;;  (emit (defn foo [a]
;;          "Docstring"
;;          (println "asdf")))
;; calls
;;  (emit-function foo "Docstring" [a] (println "asdf"))
;;
;;  (emit (test-fn 1 2 "a"))
;; calls
;;  (emit-function-call test-fn [1 2 "a"])
;;
;; Generally, the implementations of `emit` in pallet.stevedore.common, which
;; dispatch on compound types, should be sufficient for most implementations.
;;
;; The other emit-* functions are convenience functions
;; which avoid the need to reimplement all of `emit` for each Stevedore
;; implementation.

(defmulti emit-special
  "Emit a shell form as a string. Dispatched on the first element of the form."
  (fn [ & args] [*script-language* (identity (first args))]))

(defmulti emit-function
  "Emit a shell function"
  (fn [name doc? sig body] *script-language*))

(defmulti emit-function-call
  "Emit a shell function call"
  (fn [name & args] *script-language*))

(defmulti emit-infix
  (fn [type [operator & args]] *script-language*))



;; Common functions/predicates

(defn compound-form?
  "Predicate to check if expr is a compound form"
  [expr]
  (= 'do  (first expr)))

(defmulti infix-operator?
  "Predicate to check if expr is an infix operator. Each implementation
  should implement it's own multimethod."
  (fn [expr] *script-language*))



;;; * Keyword and Operator Classes
(def
  ^{:doc
    "Special forms are handled explcitly by an implementation of
     `emit-special`."
    :private true}
  special-forms
  #{'if 'if-not 'when 'when-not 'case 'aget 'aset 'get 'defn 'return 'set!
    'var 'defvar 'let 'local 'literally 'deref 'do 'str 'quoted 'apply
    'file-exists? 'directory? 'symlink? 'readable? 'writeable? 'empty?
    'not 'println 'print 'group 'pipe 'chain-or
    'chain-and 'while 'doseq 'merge! 'assoc! 'alias})


;;; Predicates for keyword/operator classes
(defn special-form?
  "Predicate to check if expr is a special form"
  [expr]
  (contains? special-forms expr))


;;; Implementation coverage tests
;;;
(defn- emit-special-implemented? [impl special-function]
  "Predicate for successfully dispatched special functions."
  (try
    (with-script-language impl
      (emit-special special-function)
      true
      (catch IllegalArgumentException e
        (not
         (.contains
          (str e)
          (str
           "No method in multimethod 'emit-special' for dispatch value: ["
           impl " " special-function "]"))))
      (catch Exception e true))))

(defn emit-special-implemented [impl]
  "Returns a vector of successfully dispatched special functions.
   Example usage:
       (emit-special-implemented :pallet.stevedore.bash/bash)"
  (filter #(emit-special-implemented? impl %) special-forms))

(defn emit-special-not-implemented [impl]
  "Returns a vector of special-functions that fail to dispatch.
       (emit-special-not-implemented :pallet.stevedore.bash/bash)"
  (remove #(emit-special-implemented? impl %) special-forms))

;;; Common implementation
(defn- ex-location [m]
  (str "(" (.getName (file (:file m))) ":" (:line m) ")"))

(defmethod emit-special [::common-impl 'invoke]
  [type form]
  (let [[fn-name-or-map & args] form]
    (tracef "INVOKE %s %s" fn-name-or-map args)
    (tracef "INVOKE %s" (meta form))
    (if (map? fn-name-or-map)
      (let [m (meta form)]
        (try
          (*script-fn-dispatch*
           fn-name-or-map (filter-empty-splice args)
           (:ns m) (or (:file m) *file*) (:line m))
          (catch clojure.lang.ArityException e
            ;; Add the script location to the error message, and use the
            ;; unmangled script function name.
            (throw
             (ex-info
              (str "Wrong number of args (" (.actual e) ") passed to: "
                   (name (:fn-name fn-name-or-map)) " " (ex-location m))
              (merge
               m
               {:actual (.actual e)
                :script-fn (:fn-name fn-name-or-map)})
              e)))
          (catch Exception e
            ;; Add the script location and script function name to the error
            ;; message
            (throw
             (ex-info
              (str (.getMessage e) " in call to "
                   (name (:fn-name fn-name-or-map)) " " (ex-location m))
              (merge
               m
               {:script-fn (:fn-name fn-name-or-map)})
              e)))))
      (let [argseq (->>
                    args
                    filter-empty-splice
                    (map emit)
                    (filter (complement string/blank?))
                    (interpose " "))]
        (apply emit-function-call fn-name-or-map argseq)))))

(defn- emit-s-expr [expr]
  (if (symbol? (first expr))
    (let [head (symbol (name (first expr))) ; remove any ns resolution
          expr1 (conj (rest expr) head)]
      (cond
       (and (= (first (str head)) \.)
            (> (count (str head)) 1)) (emit-special 'dot-method expr1)
       (special-form? head) (emit-special head expr1)
       (infix-operator? head) (emit-infix head expr1)
       :else (emit-special 'invoke expr)))
    (if (map? (first expr))
      (emit-special 'invoke expr)
      (when (seq expr)
        (string/join
         " " (filter (complement string/blank?) (map emit expr)))))))

(defn- spread
  [arglist]
  (cond
   (nil? arglist) nil
   (nil? (next arglist)) (seq (first arglist))
   :else (apply list (first arglist) (spread (next arglist)))))

(defmethod emit [::common-impl clojure.lang.IPersistentList] [expr]
  (emit-s-expr expr))

(defmethod emit [::common-impl clojure.lang.PersistentList] [expr]
  (emit-s-expr expr))

(defmethod emit [::common-impl clojure.lang.Cons]
  [expr]
  (if (= 'list (first expr))
    (emit-s-expr (rest expr))
    (emit-s-expr expr)))

(defmethod emit-special [::common-impl 'apply] [type [apply & exprs]]
  (emit-s-expr (spread exprs)))

(defmethod emit-special [::common-impl 'defn] [type [fn & expr]]
  (let [name (first expr)]
    (if (string? (second expr))
      (let [doc (second expr)
            signature (second (next expr))
            body (rest (rest (rest expr)))]
        (emit-function name doc signature body))
      (let [signature (second expr)
            body (rest (rest expr))]
        (emit-function name nil signature body)))))


;;; Script combiner implementations

(defmethod do-script ::common-impl
  [& scripts]
  (str
   (->>
    scripts
    (map #(when % (string/trim %)))
    (filter (complement string/blank?))
    (string/join \newline))
   \newline))

(defmethod chain-commands ::common-impl
  [& scripts]
  (string/join " && "
    (filter
     (complement string/blank?)
     (map #(when % (string/trim %)) scripts))))

(def ^:dynamic *status-marker* "#> ")
(def ^:dynamic *status-fail* " : FAIL")
(def ^:dynamic *status-success* " : SUCCESS")

(defn checked-start [message]
  (str "echo '" message "...';"))

(defn checked-fail [message]
  (str "echo '" *status-marker* message *status-fail* "'; exit 1;"))

(defn checked-success [message]
  (str "echo '" *status-marker* message *status-success* "'"))

(defmethod checked-commands ::common-impl
  [message & cmds]
  (let [chained-cmds (apply chain-commands cmds)]
    (if (string/blank? chained-cmds)
      ""
      (str
       (checked-start message) \newline
       "{ " chained-cmds "; } || { " (checked-fail message) " } >&2 " \newline
       (checked-success message)))))

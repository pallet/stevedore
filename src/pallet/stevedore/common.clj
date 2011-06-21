(ns pallet.stevedore.common
  (:require
    [pallet.stevedore :as stevedore]
    [clojure.string :as string]
    [clojure.contrib.seq :as c.seq]
    [clojure.contrib.logging :as logging])
  (:use
    [pallet.stevedore
     :only [emit *stevedore-impl*
            with-stevedore-impl filter-empty-splice empty-splice
            do-script chain-commands checked-commands]]))


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
  (fn [ & args] [*stevedore-impl* (identity (first args))]))

(defmulti emit-function
  "Emit a shell function"
  (fn [name doc? sig body] *stevedore-impl*))

(defmulti emit-function-call
  "Emit a shell function call"
  (fn [name & args] *stevedore-impl*))

(defmulti emit-infix
  (fn [type [operator & args]] *stevedore-impl*))



;; Common functions/predicates

(defn compound-form?
  "Predicate to check if expr is a compound form"
  [expr]
  (= 'do  (first expr)))

(defmulti infix-operator?
  "Predicate to check if expr is an infix operator. Each implementation
  should implement it's own multimethod."
  (fn [expr] *stevedore-impl*))



;;; * Keyword and Operator Classes
(def
  ^{:doc
    "Special forms are handled explcitly by an implementation of
     `emit-special`."
    :private true}
  special-forms
  #{'if 'if-not 'when 'case 'aget 'aset 'get 'defn 'return 'set! 'var 'defvar
    'let 'local 'literally 'deref 'do 'str 'quoted 'apply
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
;;; Example usage:
;;;  (emit-special-coverage :pallet.stevedore.bash/bash)
(defn emit-special-coverage [impl]
  "Returns a vector of two elements. First elements is a vector
  of successfully dispatched special functions. Second element is a vector
  of failed dispatches."
  (c.seq/separate
    (fn [s]
      (try
        (with-stevedore-impl impl
          (emit-special s)
        true
        (catch Exception e
          (not (.contains
            (str e)
            (str "java.lang.IllegalArgumentException: No method in multimethod "
                 "'emit-special' for dispatch value: [" impl " " s "]")))))))
    special-forms))



;; Common implementation
(defmethod emit-special [::common-impl 'invoke]
  [type [name & args]]
  (logging/trace (str "INVOKE " name " " args))
  (if (map? name)
    (try
      (stevedore/*script-fn-dispatch*
       name (filter-empty-splice args)
       stevedore/*script-ns* stevedore/*script-file* stevedore/*script-line*)
      (catch java.lang.IllegalArgumentException e
        (throw (java.lang.IllegalArgumentException.
                (str "Invalid arguments for " name) e))))
    (let [argseq (->>
                    args
                    filter-empty-splice
                    (map emit)
                    (filter (complement string/blank?))
                    (interpose " "))]
      (apply emit-function-call name argseq))))

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

(defmethod checked-commands ::common-impl
  [message & cmds]
  (let [chained-cmds (apply chain-commands cmds)]
    (if (string/blank? chained-cmds)
      ""
      (str
        "echo \"" message "...\"" \newline
        "{ " chained-cmds "; } || { echo \"" message "\" failed; exit 1; } >&2 "
        \newline
        "echo \"...done\"\n"))))

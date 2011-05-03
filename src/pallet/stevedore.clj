(ns pallet.stevedore
  "Embed shell script in clojure.

   Shell script is embedded by wrapping in the `script` macro.
       (script (ls)) => \"ls\"

   The result of a `script` form is a string."
  (:require
   [pallet.common.deprecate :as deprecate]
   [clojure.contrib.def :as def]
   [clojure.contrib.seq :as c.seq]
   [clojure.string :as string]
   [clojure.walk :as walk])
  (:use
   [pallet.common.string :only [underscore]]))

;;; Helper vars for parsing the stevedore DSL

(def/defunbound *stevedore-impl*
  "Current stevedore implementation")

(def/defunbound *script-ns*
  "Used to capture the namespace in which `script` is invoked.")

(def/defunbound *script-line*
  "Used to capture a form's line number.")

(def/defunbound *script-file*
  "Used to capture a form's file name.")

(defmacro with-line-number
  "Provide the source file and line number for use in reporting."
  [[file line] & body]
  `(do
     (binding [*script-line* ~line
               *script-file* ~file]
       ~@body)))

;;; Define current stevedore implementation
(def/defunbound *stevedore-impl*
  "Current stevedore implementation")

(defmacro with-stevedore-impl
  "Set which stevedore implementation to use. Currently supports:
   :pallet.stevedore.bash/bash"
  [impl & body]
  `(do
     (binding [*stevedore-impl* ~impl]
       ~@body)))

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

(defn compound-form?
  "Predicate to check if expr is a compound form"
  [expr]
  (= 'do  (first expr)))

(defmulti infix-operator?
  "Predicate to check if expr is an infix operator. Each implementation
  should implement it's own multimethod."
  (fn [expr] *stevedore-impl*))

;; Main dispatch functions
(defmulti emit-special
  "Emit a shell form as a string. Dispatched on the first element of the form."
  (fn [ & args] [*stevedore-impl* (identity (first args))]))

(defmulti emit
  "Emit a shell expression as a string. Dispatched on the :type of the
   expression."
  (fn [ expr ] [*stevedore-impl* (type expr)]))

(defmulti emit-function
  "Emit a shell function"
  (fn [name doc? sig body] *stevedore-impl*))

(defmulti emit-function-call
  "Emit a shell function call"
  (fn [name & args] *stevedore-impl*))

(defmulti emit-infix
  (fn [type [operator & args]] *stevedore-impl*))

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
            (str "java.lang.IllegalArgumentException: No method in multimethod 'emit-special' for dispatch value: [" impl " " s "]")))))))
    special-forms))


;;; Splicing functions

(defn splice-list
  "Emit a collection as a space separated list.
       (splice-list [a b c]) => \"a b c\""
  [coll]
  (if (seq coll)
    (string/join " " coll)
    ;; to maintain unquote splicing semantics, this term has to disappear
    ;; from the result
    ::empty-splice))

(defn filter-empty-splice
  [args]
  (filter #(not= ::empty-splice %) args))


;;; High level string generation functions
(def statement-separator "\n")

(defn statement
  "Emit an expression as a valid shell statement, with separator."
  [expr]
  ;; check the substring count, as it can be negative if there is a syntax issue
  ;; in a stevedore expression, and generates a cryptic error message otherwise
  (let [n (- (count expr) (count statement-separator))]
    (if (and (pos? n) (not (= statement-separator (.substring expr n))))
      (str expr statement-separator)
      expr)))


;; Common functions/predicates

(defn emit-do [exprs]
  (string/join (map (comp statement emit) (filter-empty-splice exprs))))

(defmulti emit-script
  (fn [forms] *stevedore-impl*))

(defmethod emit-script :default
  [forms]
  (let [code (if (> (count forms) 1)
               (emit-do (filter-empty-splice forms))
               (let [form (first forms)]
                 (if (= form ::empty-splice)
                   ""
                   (emit form))))]
    code))

(defn- unquote?
  "Tests whether the form is (clj ...) or (unquote ...) or ~expr."
  [form]
  (or (and (seq? form)
           (symbol? (first form))
           (= (symbol (name (first form))) 'clj))
      (and (seq? form) (= (first form) `unquote))))

(defn- unquote-splicing?
  "Tests whether the form is ~@( ...) or (unqote-splicing ...)."
  [form]
  (and (seq? form) (= (first form) `unquote-splicing)))

(defn- handle-unquote [form]
  (second form))

(defn- splice [form]
  (if (seq form)
    (string/join " " (map emit form))
    ::empty-splice))

(defn- handle-unquote-splicing [form]
  (list splice (second form)))

(declare inner-walk outer-walk)

(defn- inner-walk [form]
  (cond
   (unquote? form) (handle-unquote form)
   (unquote-splicing? form) (handle-unquote-splicing form)
   :else (walk/walk inner-walk outer-walk form)))

(defn- outer-walk [form]
  (cond
   (symbol? form) (list 'quote form)
   (seq? form) (list* 'list form)
   :else form))

(defmacro quasiquote
  [form]
  (let [post-form (walk/walk inner-walk outer-walk form)]
    post-form))

;; TODO move quausiquote to emit-script
(defmacro script
  "Takes one or more forms. Returns a string of the forms translated into
   shell script.
       (script
         (println \"hello\")
         (ls -l \"*.sh\"))"
  [& forms]
  `(with-line-number [~*file* ~(:line (meta &form))]
     (binding [*script-ns* ~*ns*]
       (emit-script (quasiquote ~forms)))))

;;; Script combiners
(defmulti do-script
  "Concatenate multiple scripts."
  (fn [& scripts] *stevedore-impl*))

(defmethod do-script :default
  [& scripts]
  (str
   (->>
    scripts
    (map #(when % (string/trim %)))
    (filter (complement string/blank?))
    (string/join \newline))
   \newline))

(defmulti chain-commands
  "Chain commands together with &&."
  (fn [& scripts] *stevedore-impl*))

(defmethod chain-commands :default
  [& scripts]
  (string/join " && "
    (filter
     (complement string/blank?)
     (map #(when % (string/trim %)) scripts))))

(defmulti checked-commands
  "Wrap a command in a code that checks the return value. Code to output the
  messages is added before the command."
  (fn [message & cmds] *stevedore-impl*))

(defmethod checked-commands :default
  [message & cmds]
  (let [chained-cmds (apply chain-commands cmds)]
    (if (string/blank? chained-cmds)
      ""
      (str
        "echo \"" message "...\"" \newline
        "{ " chained-cmds "; } || { echo \"" message "\" failed; exit 1; } >&2 "
        \newline
        "echo \"...done\"\n"))))

(defmacro chained-script
  "Takes one or more forms. Returns a string of the forms translated into a
   chained shell script command."
  [& forms]
  `(chain-commands
    ~@(map (fn [f] (list `script f)) forms)))

(defmacro checked-script
  "Takes one or more forms. Returns a string of the forms translated into
   shell scrip.  Wraps the expression in a test for the result status."
  [message & forms]
  `(checked-commands ~message
    ~@(map (fn [f] (list `script f)) forms)))

;;; Script argument helpers
;;; TODO eliminate the need for this to be public by supporting literal maps for expansion
(defn arg-string
  [option argument do-underscore do-assign dash]
  (let [opt (if do-underscore (underscore (name option)) (name option))]
    (if argument
      (if (> (.length opt) 1)
        (str dash opt (if-not (= argument true)
                        (str (if do-assign "=" " ") \" argument \")))
        (str "-" opt (if-not (= argument true) (str " " \" argument \")))))))

(defn map-to-arg-string
  "Output a set of command line switches from a map"
  [m & {:keys [underscore assign dash] :or {dash "--"}}]
  {:pre [(or (nil? m) (map? m))]}
  (apply
   str (interpose
        " "
        (map
          #(arg-string (key %) (val %) underscore assign dash)
          (filter val m)))))

(defn option-args
  "Output a set of command line switches from a sequence of options"
  [{:as m}]
  (let [assign (:assign m)
        underscore (:underscore m)]
    (map-to-arg-string
     (dissoc m :assign :underscore) :assign assign :underscore underscore)))


;; Dispatch functions for script functions

(defn script-fn-dispatch-none
  "Script function dispatch. This implementation does nothing."
  [name args ns file line]
  nil)

(def ^{:doc "Script function dispatch."}
  *script-fn-dispatch* script-fn-dispatch-none)

(defn script-fn-dispatch!
  "Set the script-fn dispatch function"
  [f]
  (alter-var-root #'*script-fn-dispatch* (fn [_] f)))

(defmacro with-no-script-fn-dispatch
  [& body]
  `(binding [*script-fn-dispatch* script-fn-dispatch-none]
     ~@body))

(defmacro with-script-fn-dispatch
  [f & body]
  `(binding [*script-fn-dispatch* ~f]
     ~@body))




;; DEPRECATED

(defmacro defimpl
  {:deprecated "0.5.0"}
  [script specialisers [& args] & body]
  (require 'pallet.script)
  `(do
     (deprecate/deprecated-macro
      ~&form
      (deprecate/rename 'pallet.stevedore/defimpl 'pallet.script/defimpl))
     (pallet.script/defimpl ~script ~specialisers [~@args] ~@body)))


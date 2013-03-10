(ns pallet.stevedore
  "Embed shell script in clojure.

   Shell script is embedded by wrapping in the `script` macro.
   (script (ls)) => \"ls\"

   The result of a `script` form is a string."
  (:require
   [clojure.java.io :as io]
   [clojure.set :refer [union]]
   [clojure.string :as string]
   [clojure.tools.logging :refer [tracef]]
   [clojure.walk :as walk]
   [pallet.common.deprecate :as deprecate]
   [pallet.common.string :refer [underscore]]))

(declare ^{:dynamic true} *script-language*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SCRIPT GENERATION PUBLIC INTERFACE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `script` is the public interface to stevedore.
;;
;; Simply pass any number of Stevedore forms to `script`, and it will return a
;; string coverting to the desired output language.
;;
;; (script
;;   (println "asdf")
;;   (println "and another"))
;;
;; To specify which implementation to use, `script` must be wrapped in
;; `with-script-language`.
;;
;; (with-script-language :pallet.stevedore.bash/bash
;;   (script
;;     (println "asdf")))

(defmacro with-script-language
  "Set which stevedore implementation to use. Currently supports:
   :pallet.stevedore.bash/bash"
  [impl & body]
  `(binding [*script-language* ~impl]
    ~@body))

(defmacro script
  "Takes one or more forms. Returns a string of the forms translated into
   shell script.

       (script
         (println \"hello\")
         (ls -l \"*.sh\"))

  Must be wrapped in `with-script-language`.  Can be wrapped in
  `with-source-line-comments` to control the generation of source line
  comments in the script."
  [& forms]
  `(emit-script (quasiquote ~forms)))

(defmacro fragment
  "Takes one or more forms. Returns a string of the forms translated into
   shell script. The returned fragment will have no source line annotations.

       (fragment
         (println \"hello\")
         (ls -l \"*.sh\"))
  Must be wrapped in `with-script-language`."
  [& forms]
  `(with-source-line-comments nil (emit-script (quasiquote ~forms))))

;;; * Keyword and Operator Classes
(def
  ^{:doc
    "Special forms are handled explcitly by an implementation of
     `emit-special`."
    :internal true}
  special-forms
  #{'if 'if-not 'when 'when-not 'case 'aget 'aset 'get 'defn 'return 'set!
    'var 'defvar 'let 'local 'literally 'deref 'do 'str 'quoted 'apply
    'file-exists? 'directory? 'symlink? 'readable? 'writeable? 'empty?
    'not 'println 'print 'group 'pipe 'chain-or
    'chain-and 'while 'doseq 'merge! 'assoc! 'alias})

(def ^:internal operators
  "Operators that should not be resolved."
  #{'+ '- '/ '* '% '== '= '< '> '<= '>= '!= '<< '>> '<<< '>>> '& '| '&& '||
    'and 'or})

(def ^:internal unresolved
  "Set of symbols that should not be resolved."
  (union special-forms operators))

;;; Public script combiners
;;;
;;; Each script argument to these functions must be wrapped in
;;; an explicit `script`.
;;;
;;; Eg. (do-script (script (ls)) (script (ls)))
;;;  => (script
;;;       (ls)
;;;       (ls))
(defmulti do-script
  "Concatenate multiple scripts."
  (fn [& scripts] *script-language*))

(defmulti chain-commands
  "Chain commands together. Commands are executed left-to-right and a command is
  only executed if the last command in the chain did not fail."
  (fn [& scripts] *script-language*))

(defmulti checked-commands
  "Wrap a command in a code that checks the return value. Code to output the
  messages is added before the command."
  (fn [message & cmds] *script-language*))

;; These functions are identical to the `*-commands` counterparts, except
;; that they take a sequence argument.

(defn do-script*
  "Concatenate multiple scripts"
  [scripts]
  (apply do-script scripts))

(defn chain-commands*
  "Chain commands together. Commands are executed left-to-right and a command is
  only executed if the last command in the chain did not fail."
  [scripts]
  (apply chain-commands scripts))

(defn checked-commands*
  "Wrap a command in a code that checks the return value. Code to output the
  messages is added before the command."
  [message scripts]
  (apply checked-commands message scripts))

;; These macros have an implicit `script` around each script argument, but
;; are otherwise identical their `*-commands` counterparts.
;;
;; Eg. (chained-script ls ls)
;;     => (script
;;          ls
;;          ls)

(defmacro chained-script
  "Takes one or more forms. Returns a string of the forms translated into a
   chained shell script command."
  [& forms]
  `(chain-commands
    ~@(map (fn [f] (with-meta (list `script f) (meta f))) forms)))

(defmacro checked-script
  "Takes one or more forms. Returns a string of the forms translated into
   shell scrip.  Wraps the expression in a test for the result status."
  [message & forms]
  `(checked-commands
    ~message
    ~@(map (fn [f] (with-meta (list `script f) (meta f))) forms)))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; IMPLEMENTATION FUNDAMENTALS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `emit` is the fundamental dispatch for stevedore implementations. It
;; dispatches on the type of its argument.
;;
;; Here is a common life cycle of a script generation.
;;
;; 1. Forms passed to `script`
;;   (script
;;     (println "abc"))
;;
;; 2. Forms are passed individually to `emit`
;;   (emit
;;     (println "abc"))
;;
;; 3. `emit` finds correct dispatch (lists are usually the initial type)
;;   (defmethod emit clojure.lang.IPersistentList
;;      [form]
;;      ...some-magic...)
;;
;;    `emit` implementations usually have recursive calls. The above function
;;    might eventually call a dispatch on java.lang.String to convert "abc".
;;
;; 4. A string results from the form.

(defmulti emit
  "Emit a shell expression as a string. Dispatched on the :type of the
   expression."
  (fn [ expr ] [*script-language* (type expr)]))

(defmethod emit :default
  [expr]
  (when-not (bound? #'*script-language*)
    (throw
     (ex-info
      "Attempting to use stevedore without specifying the target script language. Use pallet.stevedore/with-script-language to specify the target script language."
      {:expr expr})))
  (throw
   (ex-info
    (format
     "Script language %s doesn't know how to handle expressions of type %s (value is %s)."
     *script-language* (type expr) expr)
    {:script-language *script-language*
     :expr expr})))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; IMPLEMENTATION DETAILS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Helper vars and functions for parsing the stevedore DSL.

(def ^{:doc "Current stevedore implementation" :dynamic true}
  *script-language*)

;; (def ^{:doc "Used to capture the namespace in which `script` is invoked."
;;        :dynamic true}
;;   *script-ns*)

;; (def ^{:doc "Used to capture a form's line number." :dynamic true}
;;   *script-line*)

;; (def ^{:doc "Used to capture a form's file name." :dynamic true}
;;   *script-file*)

(defmacro with-line-number
  "Provide the source file and line number for use in reporting."
  [[file line] & body]
  `(binding [*script-line* ~line
            *script-file* ~file]
    ~@body))

(def ^:dynamic *apply-form-meta* true)

(defn- form-meta
  [new-form form ]
  (tracef "form-meta %s %s" form (meta form))
  (if-let [m (and *apply-form-meta* (meta form))]
    (if (number? new-form)
      new-form
      `(with-meta ~new-form ~(merge {:file *file*} (meta form))))
    new-form))

;; Preprocessing functions
;;
;; Before code forms are passed to `emit`, an initial pass is taken over them to
;; resolve unquotes, unquote-splices and other details via the macro `script`.
;;
;; These are a set of splicing utility functions.

(def
  ^{:doc "A sequence to splice"}
  splice-seq
  ::splice)

(defn splice-list
  "Mark a collection for splicing"
  [coll]
  (list ::splice coll))

(defn splice-args
  [args]
  (mapcat
   #(if (and (coll? %) (= ::splice (first %)))
      (second %)
      [%])
   args))

;; Unquote/splicing handling utility functions.
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

(defn- handle-unquote-splicing [form]
  (list `splice-list (second form)))

(def resolve-script-fns true)

;; These functions are used for an initial scan over stevedore forms
;; resolving escaping to Clojure and quoting symbols to stop namespace
;; resolution.
(defn- walk
  "Traverses form, an arbitrary data structure.  inner and outer are
  functions.  Applies inner to each element of form, building up a
  data structure of the same type, then applies outer to the result.
  Recognizes all Clojure data structures. Consumes seqs as with doall."
  [inner outer form]
  (tracef "walk %s %s %s" form (meta form) (class form))
  (cond
   (or (list? form) (instance? clojure.lang.Cons form))
   (outer (with-meta
            (if (and resolve-script-fns
                     (symbol? (first form))
                     (not (unresolved (symbol (name (first form))))))
              (list* (first form) (map inner (rest form)))
              (list* (map inner form)))
            (meta form)))

   (instance? clojure.lang.IMapEntry form) (outer (vec (map inner form)))
   (seq? form) (outer (with-meta (doall (map inner form)) (meta form)))
   (coll? form) (outer (with-meta
                         (into (empty form) (map inner form))
                         (meta form)))
   :else (outer form)))

(declare inner-walk outer-walk)

(defn- inner-walk [form]
  (tracef "inner-walk %s %s" form (meta form) (class form))
  (cond
   (unquote? form) (handle-unquote form)
   (unquote-splicing? form) (handle-unquote-splicing form)
   (instance? clojure.lang.IObj form) (with-meta
                                        (walk inner-walk outer-walk form)
                                        (meta form))
   :else (walk inner-walk outer-walk form)))

(defn- outer-walk [form]
  (tracef "outer-walk %s %s %s" form (meta form) (class form))
  (cond
   (symbol? form) (form-meta (list 'quote form) form)
   (seq? form)
   (do
     (tracef "outer-walk 2 %s %s %s" form (meta form) (class form))
     (form-meta (list* `list form) form))
   :else form))

(defn quasiquote*
  [form]
  (tracef "quasiquote* %s %s" form (meta form))
  (let [post-form (walk inner-walk outer-walk form)]
    (tracef "quasiquote return %s" post-form)
    post-form))

(defmacro quasiquote
  [form]
  (quasiquote* form))

;;; High level string generation functions
(def statement-separator "\n")

(defn script-location-comment
  [{:keys [file line]}]
  (format "    # %s:%s\n" (.getName (io/file (or file *file*))) line))

(def ^:dynamic ^:internal *src-line-comments* true)

(defmacro with-source-line-comments [flag & body]
  `(binding [*src-line-comments* ~flag]
     ~@body))

(defn statement
  "Emit an expression as a valid shell statement, with separator."
  [form script]
  ;; check the substring count, as it can be negative if there is a syntax issue
  ;; in a stevedore expression, and generates a cryptic error message otherwise
  (let [n (- (count script) (count statement-separator))
        m (meta form)]
    (if (and (pos? n) (not (= statement-separator (.substring script n))))
      (str (when (and m *src-line-comments* (not (string/blank? script)))
             (script-location-comment m))
           script
           statement-separator)
      script)))

(declare emit)

(defn emit-do [exprs]
  (->> exprs
       (map emit)
       (map statement exprs)
       string/join))

(defn emit-script
  [forms]
  (tracef "emit-script %s" forms)
  (tracef "emit-script metas %s" (vec (map meta forms)))
  (let [code (if (> (count forms) 1)
               (emit-do forms)
               (let [form (first forms)
                     m (meta form)]
                 (let [s (emit form)]
                   (str
                    (when (and m *src-line-comments* (not (string/blank? s)))
                      (script-location-comment m))
                    s))))]
    code))

;;; Script argument helpers
;;; TODO eliminate the need for this to be public by supporting literal maps for
;;; expansion
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

(ns pallet.stevedore
  "Embed shell script in clojure.

   Shell script is embedded by wrapping in the `script` macro.
   (script (ls)) => \"ls\"

   The result of a `script` form is a string."
  (:require
    [pallet.common.deprecate :as deprecate]
    [clojure.string :as string]
    [clojure.walk :as walk])
  (:use
    [pallet.common.string :only [underscore]]))

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
  Must be wrapped in `with-script-language`."
  [& forms]
  `(with-line-number [~*file* ~(:line (meta &form))]
     (binding [*script-ns* ~*ns*]
       (emit-script (quasiquote ~forms)))))


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
    ~@(map (fn [f] (list `script f)) forms)))

(defmacro checked-script
  "Takes one or more forms. Returns a string of the forms translated into
   shell scrip.  Wraps the expression in a test for the result status."
  [message & forms]
  `(checked-commands ~message
    ~@(map (fn [f] (list `script f)) forms)))






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





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; IMPLEMENTATION DETAILS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Helper vars and functions for parsing the stevedore DSL.

(def ^{:doc "Current stevedore implementation" :dynamic true}
  *script-language*)

(def ^{:doc "Used to capture the namespace in which `script` is invoked."
       :dynamic true}
  *script-ns*)

(def ^{:doc "Used to capture a form's line number." :dynamic true}
  *script-line*)

(def ^{:doc "Used to capture a form's file name." :dynamic true}
  *script-file*)

(defmacro with-line-number
  "Provide the source file and line number for use in reporting."
  [[file line] & body]
  `(do
     (binding [*script-line* ~line
               *script-file* ~file]
       ~@body)))


;; Preprocessing functions
;;
;; Before code forms are passed to `emit`, an initial pass is taken over them to
;; resolve unquotes, unquote-splices and other details via the macro `script`.
;;
;; These are a set of splicing utility functions.

(def
  ^{:doc "The empty splice"}
  empty-splice
    ::empty-splice)

(defn- splice-list
  "Emit a collection as a space separated list.
       (splice-list [a b c]) => \"a b c\""
  [coll]
  (if (seq coll)
    (string/join " " coll)
    ;; to maintain unquote splicing semantics, this term has to disappear
    ;; from the result
    empty-splice))

(defn filter-empty-splice
  [args]
  (filter #(not= empty-splice %) args))


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

(declare emit)
(defn splice [form]
  (if (seq form)
    (string/join " " (map emit form))
    empty-splice))

(defn- handle-unquote-splicing [form]
  (list `splice (second form)))


;; These functions are used for an initial scan over stevedore forms
;; resolving escaping to Clojure and quoting symbols to stop namespace
;; resolution.

(declare inner-walk outer-walk)

(defmacro quasiquote
  [form]
  (let [post-form (walk/walk inner-walk outer-walk form)]
    post-form))

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

(defn emit-do [exprs]
  (string/join (map (comp statement emit) (filter-empty-splice exprs))))

(defn emit-script
  [forms]
  (let [code (if (> (count forms) 1)
               (emit-do (filter-empty-splice forms))
               (let [form (first forms)]
                 (if (= form empty-splice)
                   ""
                   (emit form))))]
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


;; Dispatch functions for script functions

(defn script-fn-dispatch-none
  "Script function dispatch. This implementation does nothing."
  [name args ns file line]
  nil)

(def ^{:doc "Script function dispatch." :dynamic true}
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

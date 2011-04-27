(ns pallet.stevedore
  "Embed shell script in clojure.

   Shell script is embedded by wrapping in the `script` macro.
       (script (ls)) => \"ls\"

   The result of a `script` form is a string."
  (:require
   [pallet.common.deprecate :as deprecate]
   [pallet.common.resource :as resource]
   [clojure.string :as string]
   [clojure.walk :as walk]
   [clojure.contrib.condition :as condition]
   [clojure.contrib.logging :as logging]))

(defn underscore [s]
  "Change - to _"
  (string/join str "_"  (string/split s "-")))

(def
  ^{:doc "Used to capture the namespace in which `script` is invoked."
    ;:private true
    }
  *script-ns*)

(def
  ^{:doc "Used to capture a form's line number."
    ;:private true
    }
  *script-line* nil)

(def
  ^{:doc "Used to capture a form's file name."
    ;:private true
    }
  *script-file* nil)

(def
  ^{:doc "Used to store the current stevedore implementation."
    ;:private true
    }
  *stevedore-impl* nil)

(defmacro with-stevedore-impl
  "Declare which stevedore implementation to use"
  [impl & body]
  `(do
     (binding [*stevedore-impl* ~impl]
       ~@body)))

(defmacro with-line-number
  "Provide the source file and line number for use in reporting."
  [[file line] & body]
  `(do
     (binding [*script-line* ~line
               *script-file* ~file]
       ~@body)))

(defn ^String substring
  "Drops first n characters from s.  Returns an empty string if n is
  greater than the length of s."
  [n ^String s]
  (if (< (count s) n)
    ""
    (.substring s n)))

(defn ^String add-quotes
  "Add quotes to the argument s as a string"
  [s]
  (str "\"" s "\""))


(defonce
  ^{:doc
    "bash library for associative arrays in bash 3. You need to include this in
     your script if you use associative arrays, e.g. with `assoc!`."}
  hashlib (resource/slurp "stevedore/hashlib.bash"))

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


(defmulti emit
  "Emit a shell expression as a string. Dispatched on the :type of the
   expression."
  (fn [ expr ] [*stevedore-impl* (type expr)]))

(defmulti emit-special
  "Emit a shell form as a string. Dispatched on the first element of the form."
  (fn [ & args] (identity (first args))))

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

;; TODO where should this go?
(defn filter-empty-splice
  [args]
  (filter #(not= ::empty-splice %) args))

(defn emit-do [exprs]
  (string/join (map (comp statement emit) (filter-empty-splice exprs))))


(defn script* [forms]
  (let [code (if (> (count forms) 1)
               (emit-do (filter-empty-splice forms))
               (let [form (first forms)]
                 (if (= form ::empty-splice)
                   ""
                   (emit form))))]
    code))

(defmacro script
  "Takes one or more forms. Returns a string of the forms translated into
   shell script.
       (script
         (println \"hello\")
         (ls -l \"*.sh\"))"
  [& forms]
  `(with-line-number [~*file* ~(:line (meta &form))]
     (binding [*script-ns* ~*ns*]
       (script* (quasiquote ~forms)))))

;;; Script combiners
(defn do-script*
  "Concatenate multiple scripts."
  [scripts]
  (str
   (->>
    scripts
    (map #(when % (string/trim %)))
    (filter (complement string/blank?))
    (string/join \newline))
   \newline))

(defn do-script
  "Concatenate multiple scripts."
  [& scripts]
  (do-script* scripts))

(defn chain-commands*
  "Chain commands together with &&."
  [scripts]
  (string/join " && "
    (filter
     (complement string/blank?)
     (map #(when % (string/trim %)) scripts))))

(defn chain-commands
  "Chain commands together with &&."
  [& scripts]
  (chain-commands* scripts))

(defn checked-commands*
  "Wrap a command in a code that checks the return value. Code to output the
  messages is added before the command."
  [message cmds]
  (let [chained-cmds (chain-commands* cmds)]
    (if (string/blank? chained-cmds)
      ""
      (str
        "echo \"" message "...\"" \newline
        "{ " chained-cmds "; } || { echo \"" message "\" failed; exit 1; } >&2 "
        \newline
        "echo \"...done\"\n"))))

(defn checked-commands
  "Wrap a command in a code that checks the return value. Code to output the
  messages is added before the command."
  [message & cmds]
  (checked-commands* message cmds))

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


;;; script argument helpers
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

(defmacro defimpl
  {:deprecated "0.5.0"}
  [script specialisers [& args] & body]
  (require 'pallet.script)
  `(do
     (deprecate/deprecated-macro
      ~&form
      (deprecate/rename 'pallet.stevedore/defimpl 'pallet.script/defimpl))
     (pallet.script/defimpl ~script ~specialisers [~@args] ~@body)))


(ns pallet.stevedore.bash
  (:require
    [pallet.common.resource :as resource]
    [pallet.common.string :as common-string]
    [slingshot.core :as slingshot]
    [clojure.string :as string])
  (:use
    [pallet.stevedore.common]
    [pallet.stevedore
     :only [emit emit-do *script-fn-dispatch* empty-splice]]
    [pallet.common.string :only [quoted substring underscore]]))


(derive ::bash :pallet.stevedore.common/common-impl)

;;; * Keyword and Operator Classes
(def infix-operators
  ^{:doc "Operators that should be converted to infix in expressions."
    :private true}
  #{'+ '- '/ '* '% '== '= '< '> '<= '>= '!= '<< '>> '<<< '>>> '& '| '&& '||
    'and 'or})

(def logical-operators
  ^{:doc "Logical operators for test expressions."
    :private true}
  #{'== '= '< '> '<= '>= '!= '<< '>> '<<< '>>> '& '| '&& '||
    'file-exists? 'directory? 'symlink? 'readable? 'writeable? 'empty?
    'not 'and 'or})

(def
  ^{:doc "Operators that should quote their arguments."
    :private true}
  quoted-operators
  (disj logical-operators 'file-exists? 'directory? 'symlink 'can-read 'empty?))

(def
  ^{:doc "Conversion from clojure operators to shell infix operators."
    :private true}
  infix-conversions
     {'&& "-a"
      'and "-a"
      '|| "-o"
      'or "-o"
      '< "\\<"
      '> "\\>"
      '= "=="})

;; Helper functions for generating shFlags declarations
;; and initializations

(defn- shflags-declare [type long short doc default]
  "Helper for shFlags flag declarations"
  (str "DEFINE_" (name type) " "
       (apply str (interpose " " (map quoted [long default doc short])))
       "\n"))

(defn- shflags-doc-string [doc]
  (assert (string? doc))
  (str "FLAGS_HELP=" (quoted doc) "\n"))

(defn- shflags-setup []
  (str "FLAGS \"$@\" || exit 1\n"
       "eval set -- \"${FLAGS_ARGV}\"\n"))

(defn- deconstruct-sig [sig]
  "Returns a vector with the first element being a vector
  of arguments and second being a vector of flags"
  (assert (vector? sig))
  (let [[args flags :as dsig] (split-with symbol? sig)]
    (assert (or (empty? flags) (every? vector? flags)))
    dsig))

(defn shflags-make-declaration [doc? sig]
  "Returns a shflags declaration"
  (let [[args flags] (deconstruct-sig sig)]
    (str (when (string? doc?)
           (str (shflags-doc-string doc?)))
         (when (seq flags)
           (str (apply str (map (partial apply shflags-declare) flags))))
         (shflags-setup)
         (when (seq args)
           (str
             (string/join
              "\n" (map #(str (emit %1) "=" "$" %2) args (iterate inc 1)))
             \newline)))))


(defmethod infix-operator? ::bash [expr]
  (contains? infix-operators expr))

(defn- logical-operator?
  "Predicate to check if expr is a logical operator"
  [expr]
  (contains? logical-operators expr))

(defn- quoted-operator?
  "Predicate to check if expr is a quoted operator"
  [expr]
  (contains? quoted-operators expr))

(defn- logical-test? [test]
  (and (sequential? test)
       (or (infix-operator? (first test))
           (logical-operator? (first test)))))

;;; Emit special forms
(defn- emit-quoted-if-not-subexpr [f expr]
  (let [s (emit expr)]
    (if (or (.startsWith s "\\(")
            (.startsWith s "!")
            (.startsWith s "-")
            (.startsWith s "@"))
      s
      (f s))))

(defmethod emit-infix ::bash [type [operator & args]]
  (when (< (count args) 2)
    (throw (Exception. "Less than 2 infix arguments not supported yet.")))
  (let [open (if (logical-operator? operator) "\\( " "(")
        close (if (logical-operator? operator) " \\)" ")")
        quoting (if (quoted-operator? operator) quoted identity)]
    (str open (emit-quoted-if-not-subexpr quoting (first args)) " "
         (get infix-conversions operator operator)
         " " (emit-quoted-if-not-subexpr quoting (second args)) close)))


(defmethod emit-special [::bash 'file-exists?] [type [file-exists? path]]
  (str "-e " (emit path)))

(defmethod emit-special [::bash 'directory?] [type [directory? path]]
  (str "-d " (emit path)))

(defmethod emit-special [::bash 'symlink?] [type [symlink? path]]
  (str "-h " (emit path)))

(defmethod emit-special [::bash 'readable?] [type [readable? path]]
  (str "-r " (emit path)))

(defmethod emit-special [::bash 'writeable?] [type [readable? path]]
  (str "-w " (emit path)))

(defmethod emit-special [::bash 'empty?] [type [empty? path]]
  (str "-z " (emit path)))

(defmethod emit-special [::bash 'not] [type [not expr]]
  (str "! " (emit expr)))

(defmethod emit-special [::bash 'local] [type [local name expr]]
  (str "local " (emit name) "=" (emit expr)))

(defn- check-symbol [var-name]
  (when (re-matches #".*-.*" var-name)
    (slingshot/throw+
     {:type :invalid-bash-symbol
      :message (format "Invalid bash symbol %s" var-name)}))
  var-name)

(defn- munge-symbol [var-name]
  (let [var-name (string/replace var-name "-" "__")
        var-name (string/replace var-name "." "_DOT_")
        var-name (string/replace var-name "/" "_SLASH_")]
    var-name))

(defn- set-map-values
  [var-name m]
  (str "{ "
         (string/join ""
          (map
           #(format "hash_set %s %s %s; "
                    (munge-symbol (emit var-name))
                    (munge-symbol (emit (first %)))
                    (emit (second %)))
           m))
         " }"))

    ;; This requires bash 4
    ;; (str
    ;;  "{ "
    ;;  "declare -a " (emit var-name) "; "
    ;;  (check-symbol (emit var-name)) "=" (emit expr)
    ;;  "; }")

(defmethod emit-special [::bash 'var] [type [var var-name expr]]
  (if (instance? clojure.lang.IPersistentMap expr)
    (set-map-values var-name expr)
    (str
     (check-symbol (emit var-name)) "=" (emit expr))))

(defmethod emit-special [::bash 'defvar] [type [defvar name expr]]
  (str (emit name) "=" (emit expr)))

(defmethod emit-special [::bash 'let] [type [let name expr]]
  (str "let " (emit name) "=" (emit expr)))

(defmethod emit-special [::bash 'alias] [type [alias name expr]]
  (str "alias " (emit name) "='" (emit expr) "'"))

(defmethod emit-special [::bash 'str] [type [str & args]]
  (apply clojure.core/str (map emit args)))

(defmethod emit-special [::bash 'quoted] [type [quoted arg]]
  (common-string/quoted (emit arg)))

(defmethod emit-special [::bash 'println] [type [println & args]]
  (str "echo " (emit args)))

(defmethod emit-special [::bash 'print] [type [println & args]]
  (str "echo -n " (emit args)))


(defonce
  ^{:doc
    "bash library for associative arrays in bash 3. You need to include this in
     your script if you use associative arrays, e.g. with `assoc!`."}
  hashlib (resource/slurp "stevedore/hashlib.bash"))


(defmethod emit [::bash nil] [expr]
  "null")

(defmethod emit [::bash java.lang.Integer] [expr]
  (str expr))

(defmethod emit [::bash clojure.lang.Ratio] [expr]
  (str (float expr)))

(defmethod emit [::bash clojure.lang.Keyword] [expr]
  (name expr))

(defmethod emit [::bash java.lang.String] [expr]
  expr)

(defmethod emit [::bash clojure.lang.Symbol] [expr]
  (str expr))

(defmethod emit [::bash java.lang.Boolean] [expr]
  (str expr))

;; TODO should this even exist?
;; It causes seemingly unnessessary conflicts with ::common-impl implementations
;; we don't buy much by having it.
;;
;;(defmethod emit [::bash java.lang.Object] [expr]
;;  (str expr))

(defmethod emit [::bash empty-splice] [expr]
  "")

(defmethod emit [::bash clojure.lang.IPersistentVector] [expr]
  (str "(" (string/join " " (map emit expr)) ")"))

(defmethod emit [::bash clojure.lang.IPersistentMap] [expr]
  (letfn [(subscript-assign
           [pair]
           (str "["(emit (key pair)) "]=" (emit (val pair))))]
    (str "(" (string/join " " (map subscript-assign (seq expr))) ")")))

;;; TODO move to pallet.common.string
(defn comma-list
  "Emit a collection as a parentesised, comma separated list.
       (comma-list [a b c]) => \"(a, b, c)\""
  [coll]
  (str "(" (string/join ", " coll) ")"))




(defn emit-method [obj method args]
  (str (emit obj) "." (emit method) (comma-list (map emit args))))

(defn- emit-body-for-if [form]
  (if (or (compound-form? form)
          (= 'if (first form))
          (.contains (emit form) "\n"))
    (str \newline (string/trim (emit form)) \newline)
    (str " " (emit form) ";")))

(defmethod emit-special [::bash 'if] [type [if test true-form & false-form]]
  (str "if "
       (if (logical-test? test) (str "[ " (emit test) " ]") (emit test))
       "; then"
       (emit-body-for-if true-form)
       (when (first false-form)
         (str "else" (emit-body-for-if (first false-form))))
       "fi"))

(defmethod emit-special [::bash 'if-not] [type [if test true-form & false-form]]
  (str "if "
       (if (logical-test? test)
         (str "[ ! " (emit test) " ]")
         (str "! " (emit test)))
       "; then"
       (emit-body-for-if true-form)
       (when (first false-form)
         (str "else" (emit-body-for-if (first false-form))))
       "fi"))

(defmethod emit-special [::bash 'case]
  [type [case test & exprs]]
  (str "case " (emit test) " in\n"
       (string/join ";;\n"
        (map #(str (emit (first %)) ")\n" (emit (second %)))
             (partition 2 exprs)))
       ";;\nesac"))

(defmethod emit-special [::bash 'dot-method] [type [method obj & args]]
  (let [method (symbol (substring (str method) 1))]
    (emit-method obj method args)))

(defmethod emit-special [::bash 'return] [type [return expr]]
  (str "return " (emit expr)))

(defmethod emit-special [::bash 'set!] [type [set! var val]]
  (str (check-symbol (emit var)) "=" (emit val)))

(defmethod emit-special [::bash 'new] [type [new class & args]]
  (str "new " (emit class) (comma-list (map emit args))))

(defmethod emit-special [::bash 'aget] [type [aget var idx]]
  (str "${" (emit var) "[" (emit idx) "]}"))

(defmethod emit-special [::bash 'get] [type [get var-name idx]]
  (str "$(hash_echo "
       (munge-symbol (emit var-name)) " "
       (munge-symbol (emit idx))
       " -n )"))

(defmethod emit-special [::bash 'aset] [type [aget var idx val]]
  (str (emit var) "[" (emit idx) "]=" (emit val)))

(defmethod emit-special [::bash 'merge!] [type [merge! var-name expr]]
  (set-map-values var-name expr))

(defmethod emit-special [::bash 'assoc!] [type [merge! var-name idx val]]
  (format
   "hash_set %s %s %s"
   (munge-symbol (emit var-name))
   (munge-symbol (emit idx))
   (emit val)))

(defmethod emit-special [::bash 'deref]
  [type [deref expr]]
  (if (instance? clojure.lang.IPersistentList expr)
    (str "$(" (emit expr) ")")
    (str "${" (emit expr) "}")))


(defmethod emit-special [::bash 'do] [type [ do & exprs]]
  (emit-do exprs))

(defmethod emit-special [::bash 'when] [type [when test & form]]
  (str "if "
       (if (logical-test? test) (str "[ " (emit test) " ]") (emit test))
       "; then"
       (str \newline (string/trim (emit-do form)) \newline)
       "fi"))

(defmethod emit-special [::bash 'while]
  [type [ while test & exprs]]
  (str "while "
       (if (logical-test? test) (str "[ " (emit test) " ]") (emit test))
       "; do\n"
       (emit-do exprs)
       "done\n"))

(defmethod emit-special [::bash 'doseq]
  [type [ doseq [arg values] & exprs]]
  (str "for " (emit arg) " in " (string/join " " (map emit values))
       "; do\n"
       (emit-do exprs)
       "done"))

(defmethod emit-special [::bash 'group]
  [type [ group & exprs]]
  (str "{ " (string/join "; " (map emit exprs)) "; }"))

(defmethod emit-special [::bash 'pipe]
  [type [ pipe & exprs]]
  (string/join " | " (map emit exprs)))

(defmethod emit-special [::bash 'chain-or]
  [type [chain-or & exprs]]
  (string/join " || " (map emit exprs)))

(defmethod emit-special [::bash 'chain-and]
  [type [chain-and & exprs]]
  (string/join " && " (map emit exprs)))

(defmethod emit-function ::bash
  [name doc? sig body]
  (assert (symbol? name))
  (str name "() {\n"
       (shflags-make-declaration doc? sig)
       (emit-do body)
       "}\n"))

(defmethod emit-function-call ::bash
  [name & args]
  (if (seq args)
    (apply str (emit name) " " args)
    (emit name)))

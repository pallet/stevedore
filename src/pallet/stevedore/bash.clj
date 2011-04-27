(ns pallet.stevedore.bash
  (:require [clojure.string :as string]
            [pallet.common.resource :as resource]
            [clojure.contrib.condition :as condition]
            [clojure.contrib.logging :as logging])
  (:use [pallet.stevedore :only [emit add-quotes *script-fn-dispatch* *script-line* *script-ns* *script-file*]]))

;;; shFlags helpers
(defn- shflags-declare [type long short doc default]
  "Helper for shFlags flag declarations"
  (str "DEFINE_" (name type) " "
       (apply str (interpose " " (map add-quotes [long default doc short])))
       "\n"))

(defn- shflags-doc-string [doc]
  (assert (string? doc))
  (str "FLAGS_HELP=" (add-quotes doc) "\n"))

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
             (string/join "\n" (map #(str (emit %1) "=" "$" %2) args (iterate inc 1)))
             \newline)))))

(defonce
  ^{:doc
    "bash library for associative arrays in bash 3. You need to include this in
     your script if you use associative arrays, e.g. with `assoc!`."}
  hashlib (resource/slurp "stevedore/hashlib.bash"))

(def statement-separator "\n")

(defmethod emit [:bash nil] [expr]
  "null")

(defmethod emit [:bash java.lang.Integer] [expr]
  (str expr))

(defmethod emit [:bash clojure.lang.Ratio] [expr]
  (str (float expr)))

(defmethod emit [:bash clojure.lang.Keyword] [expr]
  (name expr))

(defmethod emit [:bash java.lang.String] [expr]
  expr)

(defmethod emit [:bash clojure.lang.Symbol] [expr]
  (str expr))

(defmethod emit [:bash :default] [expr]
  (str expr))

(defn comma-list
  "Emit a collection as a parentesised, comma separated list.
       (comma-list [a b c]) => \"(a, b, c)\""
  [coll]
  (str "(" (string/join ", " coll) ")"))

(defn splice-list
  "Emit a collection as a space separated list.
       (splice-list [a b c]) => \"a b c\""
  [coll]
  (if (seq coll)
    (string/join " " coll)
    ;; to maintain unquote splicing semantics, this term has to disappear
    ;; from the result
    ::empty-splice))

(defmethod emit [:bash ::empty-splice] [expr]
  "")

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

;;; Predicates for keyword/operator classes
(defn- special-form?
  "Predicate to check if expr is a special form"
  [expr]
  (contains? special-forms expr))

(defn- compound-form?
  "Predicate to check if expr is a compound form"
  [expr]
  (= 'do  (first expr)))

(defn- infix-operator?
  "Predicate to check if expr is an infix operator"
  [expr]
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

(defn- emit-infix [type [operator & args]]
  (when (< (count args) 2)
    (throw (Exception. "Less than 2 infix arguments not supported yet.")))
  (let [open (if (logical-operator? operator) "\\( " "(")
        close (if (logical-operator? operator) " \\)" ")")
        quoting (if (quoted-operator? operator) add-quotes identity)]
    (str open (emit-quoted-if-not-subexpr quoting (first args)) " "
         (get infix-conversions operator operator)
         " " (emit-quoted-if-not-subexpr quoting (second args)) close)))

(defmulti emit-special
  "Emit a shell form as a string. Dispatched on the first element of the form."
  (fn [ & args] (identity (first args))))

(defmethod emit-special 'file-exists? [type [file-exists? path]]
  (str "-e " (emit path)))

(defmethod emit-special 'directory? [type [directory? path]]
  (str "-d " (emit path)))

(defmethod emit-special 'symlink?
  [type [symlink? path]]
  (str "-h " (emit path)))

(defmethod emit-special 'readable?
  [type [readable? path]]
  (str "-r " (emit path)))

(defmethod emit-special 'writeable?
  [type [readable? path]]
  (str "-w " (emit path)))

(defmethod emit-special 'empty?
  [type [empty? path]]
  (str "-z " (emit path)))

(defmethod emit-special 'not [type [not expr]]
  (str "! " (emit expr)))

(defmethod emit-special 'local [type [local name expr]]
  (str "local " (emit name) "=" (emit expr)))

(defn- check-symbol [var-name]
  (when (re-matches #".*-.*" var-name)
    (condition/raise
     :type :invalid-bash-symbol
     :message (format "Invalid bash symbol %s" var-name)))
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

(defmethod emit-special 'var [type [var var-name expr]]
  (if (instance? clojure.lang.IPersistentMap expr)
    (set-map-values var-name expr)
    (str
     (check-symbol (emit var-name)) "=" (emit expr))))

(defmethod emit-special 'defvar [type [defvar name expr]]
  (str (emit name) "=" (emit expr)))

(defmethod emit-special 'let [type [let name expr]]
  (str "let " (emit name) "=" (emit expr)))

(defmethod emit-special 'alias [type [alias name expr]]
  (str "alias " (emit name) "='" (emit expr) "'"))

(defmethod emit-special 'str [type [str & args]]
  (apply clojure.core/str (map emit args)))

(defmethod emit-special 'quoted [type [quoted arg]]
  (add-quotes (emit arg)))

(defmethod emit-special 'println [type [println & args]]
  (str "echo " (emit args)))

(defmethod emit-special 'print [type [println & args]]
  (str "echo -n " (emit args)))


(defn emit-method [obj method args]
  (str (emit obj) "." (emit method) (comma-list (map emit args))))

(defn- emit-body-for-if [form]
  (if (or (compound-form? form)
          (= 'if (first form))
          (.contains (emit form) "\n"))
    (str \newline (string/trim (emit form)) \newline)
    (str " " (emit form) ";")))

(defmethod emit-special 'if [type [if test true-form & false-form]]
  (str "if "
       (if (logical-test? test) (str "[ " (emit test) " ]") (emit test))
       "; then"
       (emit-body-for-if true-form)
       (when (first false-form)
         (str "else" (emit-body-for-if (first false-form))))
       "fi"))

(defmethod emit-special 'if-not [type [if test true-form & false-form]]
  (str "if "
       (if (logical-test? test)
         (str "[ ! " (emit test) " ]")
         (str "! " (emit test)))
       "; then"
       (emit-body-for-if true-form)
       (when (first false-form)
         (str "else" (emit-body-for-if (first false-form))))
       "fi"))

(defmethod emit-special 'case
  [type [case test & exprs]]
  (str "case " (emit test) " in\n"
       (string/join ";;\n"
        (map #(str (emit (first %)) ")\n" (emit (second %)))
             (partition 2 exprs)))
       ";;\nesac"))

(defmethod emit-special 'dot-method [type [method obj & args]]
  (let [method (symbol (pallet.stevedore/substring (str method) 1))]
    (emit-method obj method args)))

(defmethod emit-special 'return [type [return expr]]
  (str "return " (emit expr)))

(defmethod emit-special 'set! [type [set! var val]]
  (str (check-symbol (emit var)) "=" (emit val)))

(defmethod emit-special 'new [type [new class & args]]
  (str "new " (emit class) (comma-list (map emit args))))

(defmethod emit-special 'aget [type [aget var idx]]
  (str "${" (emit var) "[" (emit idx) "]}"))

(defmethod emit-special 'get [type [get var-name idx]]
  (str "$(hash_echo "
       (munge-symbol (emit var-name)) " "
       (munge-symbol (emit idx))
       " -n )"))

(defmethod emit-special 'aset [type [aget var idx val]]
  (str (emit var) "[" (emit idx) "]=" (emit val)))

(defmethod emit-special 'merge! [type [merge! var-name expr]]
  (set-map-values var-name expr))

(defmethod emit-special 'assoc! [type [merge! var-name idx val]]
  (format
   "hash_set %s %s %s"
   (munge-symbol (emit var-name))
   (munge-symbol (emit idx))
   (emit val)))

(defmethod emit-special 'deref
  [type [deref expr]]
  (if (instance? clojure.lang.IPersistentList expr)
    (str "$(" (emit expr) ")")
    (str "${" (emit expr) "}")))


(defmethod emit-special 'do [type [ do & exprs]]
  (pallet.stevedore/emit-do exprs))

(defmethod emit-special 'when [type [when test & form]]
  (str "if "
       (if (logical-test? test) (str "[ " (emit test) " ]") (emit test))
       "; then"
       (str \newline (string/trim (pallet.stevedore/emit-do form)) \newline)
       "fi"))

(defmethod emit-special 'while
  [type [ while test & exprs]]
  (str "while "
       (if (logical-test? test) (str "[ " (emit test) " ]") (emit test))
       "; do\n"
       (pallet.stevedore/emit-do exprs)
       "done\n"))

(defmethod emit-special 'doseq
  [type [ doseq [arg values] & exprs]]
  (str "for " (emit arg) " in " (string/join " " (map emit values))
       "; do\n"
       (pallet.stevedore/emit-do exprs)
       "done"))

(defmethod emit-special 'group
  [type [ group & exprs]]
  (str "{ " (string/join "; " (map emit exprs)) "; }"))

(defmethod emit-special 'pipe
  [type [ pipe & exprs]]
  (string/join " | " (map emit exprs)))

(defmethod emit-special 'chain-or
  [type [chain-or & exprs]]
  (string/join " || " (map emit exprs)))

(defmethod emit-special 'chain-and
  [type [chain-and & exprs]]
  (string/join " && " (map emit exprs)))

(defn- emit-function [name doc? sig body]
  (assert (symbol? name))
  (str name "() {\n"
       (shflags-make-declaration doc? sig)
       (pallet.stevedore/emit-do body)
       "}\n"))

(defmethod emit-special 'defn [type [fn & expr]]
  (let [name (first expr)]
    (if (string? (second expr))
      (let [doc (second expr)
            signature (second (next expr))
            body (rest (rest (rest expr)))]
        (emit-function name doc signature body))
      (let [signature (second expr)
            body (rest (rest expr))]
        (emit-function name nil signature body)))))

(defn emit-s-expr [expr]
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
        (string/join " " (filter (complement string/blank?) (map emit expr)))))))

(defmethod emit [:bash clojure.lang.IPersistentList] [expr]
  (emit-s-expr expr))

(defmethod emit [:bash clojure.lang.Cons]
  [expr]
  (if (= 'list (first expr))
    (emit-s-expr (rest expr))
    (emit-s-expr expr)))

(defn- spread
  [arglist]
  (cond
   (nil? arglist) nil
   (nil? (next arglist)) (seq (first arglist))
   :else (apply list (first arglist) (spread (next arglist)))))

(defmethod emit-special 'apply [type [apply & exprs]]
  (emit-s-expr (spread exprs)))

(defmethod emit [:bash clojure.lang.IPersistentVector] [expr]
  (str "(" (string/join " " (map emit expr)) ")"))

(defmethod emit [:bash clojure.lang.IPersistentMap] [expr]
  (letfn [(subscript-assign
           [pair]
           (str "["(emit (key pair)) "]=" (emit (val pair))))]
    (str "(" (string/join " " (map subscript-assign (seq expr))) ")")))

(defmethod emit-special 'invoke
  [type [name & args]]
  (logging/trace (str "INVOKE " name " " args))
  (if (map? name)
    (try
      (*script-fn-dispatch*
       name (pallet.stevedore/filter-empty-splice args) *script-ns* *script-file* *script-line*)
      (catch java.lang.IllegalArgumentException e
        (throw (java.lang.IllegalArgumentException.
                (str "Invalid arguments for " name) e))))
    (let [argseq (->>
                    args
                    pallet.stevedore/filter-empty-splice
                    (map emit)
                    (filter (complement string/blank?))
                    (interpose " "))]
      (if (seq argseq)
        (apply str (emit name) " " argseq)
        (emit name)))))

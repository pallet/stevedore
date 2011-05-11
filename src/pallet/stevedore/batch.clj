(ns pallet.stevedore.batch
  (:require
    [clojure.contrib.condition :as condition]
    [clojure.string :as string])
  (:use
    [pallet.stevedore.common]
    [pallet.stevedore 
     :only [emit emit-do]]))

(derive ::batch :pallet.stevedore.common/common-impl)

;;; * Keyword and Operator Classes
(def
  ^{:doc "Conversion from clojure operators to shell infix operators."
    :private true}
  infix-conversions
     {'+ "+"
      '* "*"
      '/ "/"
      '- "-"
      '< "LSS"
      '<= "LEQ"
      '> "GTR"
      '>= "GEQ"
      '= "EQU"
      '!= "NEQ"
      '== "EQU"
      '% "%%"})

(defmethod infix-operator? ::batch [expr]
  (contains? infix-conversions expr))

(defmethod emit-infix ::batch [type [operator & args]]
  (when (< (count args) 2)
    (throw (Exception. "Less than 2 infix arguments not supported yet.")))
  (let [open "("
        close ")"]
    (str open (emit (first args)) " "
         (get infix-conversions operator operator)
         " " (emit (second args)) close)))

(defmethod emit [::batch java.lang.Integer] [expr]
  (str expr))

(defmethod emit [::batch clojure.lang.Ratio] [expr]
  (str (float expr)))

(defmethod emit [::batch clojure.lang.Symbol] [expr]
  (str expr))

;; TODO copied from bash shflags, consolidate into common library
(defn- deconstruct-sig [sig]
  "Returns a vector with the first element being a vector
  of arguments and second being a vector of flags"
  (assert (vector? sig))
  (let [[args flags :as dsig] (split-with symbol? sig)]
    (assert (or (empty? flags) (every? vector? flags)))
    dsig))

(defmethod emit-function ::batch
  [name doc? sig body]
  (assert (symbol? name))
  (let [[args flags] (deconstruct-sig sig)]
    (str ":" name "\n"
         "SETLOCAL\n"
         (when (seq args)
           (str
             (string/join "\n" (map #(str "set " (emit %1) "=" "%~" %2 "%") args (iterate inc 1)))
             \newline))
         (emit-do body)
         "GOTO :EOF")))

(defmethod emit-function-call ::batch
  [name & args]
  (if (seq args)
    (apply str "call:" (emit name) " " args)
    (str "call:" (emit name))))

(defn- check-symbol [var-name]
  (when (re-matches #".*-.*" var-name)
    (condition/raise
     :type :invalid-bash-symbol
     :message (format "Invalid batch symbol %s" var-name)))
  var-name)

(defmethod emit-special [::batch 'set!] [type [set! var val]]
  (str "set " (check-symbol (emit var)) "=" (emit val)))
(defmethod emit-special [::batch 'var] [type [set! var val]]
  (str "set " (check-symbol (emit var)) "=" (emit val)))

(defmethod emit [::batch java.lang.String] [expr]
  expr)

(defmethod emit-special [::batch 'str] [type [str & args]]
  (apply clojure.core/str (map emit args)))

(defmethod emit-special [::batch 'println] [type [println & args]]
  (str "echo " (emit args)))

(defmethod emit-special [::batch 'deref]
  [type [deref expr]]
  (str "%" (emit expr) "%"))

(defmethod emit-special [::batch 'group]
  [type [ group & exprs]]
  (str "(\n" (string/join "\n" (map emit exprs)) "\n)"))

(defmethod emit-special [::batch 'do] [type [ do & exprs]]
  (emit-do exprs))

(defn- gen-if-statement [if test true-form false-form]
  (str if
       (emit test)
       " (\n"
       (emit true-form)
       (when (seq false-form)
         (str "\n) else (\n" (emit false-form)))
       "\n)"))

(defmethod emit-special [::batch 'if] [type [if test true-form & false-form]]
  (gen-if-statement "if " test true-form false-form))

(defmethod emit-special [::batch 'if-not] [type [if test true-form & false-form]]
  (gen-if-statement "if NOT " test true-form false-form))

(defmethod emit-special [::batch 'when] [type [when test & form]]
  (gen-if-statement "if " test (cons 'do form) nil))

(defmethod emit-special [::batch 'file-exists?] [type [file-exists? path]]
  (str "EXIST " (emit path)))

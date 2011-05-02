(ns pallet.stevedore.batch
  (:require
    [clojure.contrib.condition :as condition]
    pallet.stevedore.common)
  (:use
    [pallet.stevedore 
     :only [emit emit-special emit-function-call emit-do emit-function emit-infix
            infix-operator?]]
    [pallet.stevedore :only [emit]]))

(derive ::batch :pallet.stevedore.common/common-impl)

;;; * Keyword and Operator Classes
(def infix-operators
  ^{:doc "Operators that should be converted to infix in expressions."
    :private true}
  #{'+ '- '/ '* '% '== '= '< '> '<= '>= '!= '<< '>> '<<< '>>> '& '| '&& '||
    'and 'or})

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

(defmethod infix-operator? ::batch [expr]
  (contains? infix-operators expr))

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

(defmethod emit-function ::batch
  [name doc? sig body]
  (assert (symbol? name))
  (str ":" name "\n"
       (emit-do body)
       "GOTO :EOF"))

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

(defmethod emit [::batch java.lang.String] [expr]
  expr)

(defmethod emit-special [::batch 'str] [type [str & args]]
  (apply clojure.core/str (map emit args)))

(defmethod emit-special [::batch 'println] [type [println & args]]
  (str "echo " (emit args)))

(defmethod emit-special [::batch 'deref]
  [type [deref expr]]
  (str "%" (emit expr) "%"))

(ns pallet.stevedore.batch
  (:require
    pallet.stevedore.common)
  (:use
    [pallet.stevedore :only [compound-form? special-form? emit emit-special emit-do emit-function splice-list *script-fn-dispatch* infix-operator? emit-infix]]
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
  (let [open "( "
        close " )"]
    (str open (emit (first args)) " "
         (get infix-conversions operator operator)
         " " (emit (second args)) close)))

(defmethod emit [::batch java.lang.Integer] [expr]
  (str expr))

(defmethod emit [::batch clojure.lang.Ratio] [expr]
  (str (float expr)))

(defmethod emit [::batch clojure.lang.Symbol] [expr]
  (str expr))

(defmethod emit-function ::bash
  [name doc? sig body]
  (assert (symbol? name))
  (str ":" name "\n"
       (emit-do body)
       "GOTO :EOF"))

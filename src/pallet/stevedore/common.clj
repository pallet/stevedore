(ns pallet.stevedore.common
  (:require 
    [pallet.stevedore :as stevedore]
    [clojure.string :as string]
    [clojure.contrib.seq :as c.seq]
    [clojure.contrib.logging :as logging])
  (:use
    [pallet.stevedore 
     :only [emit emit-special emit-function emit-infix emit-function-call
            with-stevedore-impl compound-form? infix-operator?
            do-script chain-commands checked-commands]]))


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
            (str "java.lang.IllegalArgumentException: No method in multimethod 'emit-special' for dispatch value: [" impl " " s "]")))))))
    special-forms))



;; Common implementation
(defmethod emit-special [::common-impl 'invoke]
  [type [name & args]]
  (logging/trace (str "INVOKE " name " " args))
  (if (map? name)
    (try
      (stevedore/*script-fn-dispatch*
       name (stevedore/filter-empty-splice args) stevedore/*script-ns* stevedore/*script-file* stevedore/*script-line*)
      (catch java.lang.IllegalArgumentException e
        (throw (java.lang.IllegalArgumentException.
                (str "Invalid arguments for " name) e))))
    (let [argseq (->>
                    args
                    stevedore/filter-empty-splice
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
        (string/join " " (filter (complement string/blank?) (map emit expr)))))))

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


;;; Script combiners

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

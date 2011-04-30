(ns pallet.stevedore.common
  (:require 
    [pallet.stevedore :as stevedore]
    [clojure.string :as string]
    [clojure.contrib.logging :as logging])
  (:use
    [pallet.stevedore 
     :only [emit emit-special emit-function emit-infix emit-function-call
            special-form? compound-form? infix-operator?]]))


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

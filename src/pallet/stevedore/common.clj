(ns pallet.stevedore.common
  (:require 
    [pallet.stevedore :as stevedore]
    [clojure.string :as string]
    [clojure.contrib.logging :as logging])
  (:use
    [pallet.stevedore :only [emit emit-special]]))



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
      (if (seq argseq)
        (apply str (emit name) " " argseq)
        (emit name)))))

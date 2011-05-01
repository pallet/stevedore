(ns pallet.stevedore.batch-test
  (:use
   [pallet.common.string :only [quoted]]
   pallet.stevedore 
   pallet.stevedore.batch
   midje.sweet
   clojure.test))

(deftest number-literal
  (with-stevedore-impl :pallet.stevedore.batch/batch
    (facts 
      (script 42) => "42"
      (script 1/2) => "0.5")))

(deftest simple-call-test
  (with-stevedore-impl :pallet.stevedore.batch/batch
    (fact
      (script (a b)) => "call:a b"
      (script (a)) => "call:a")))

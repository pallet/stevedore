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

(deftest test-string
  (with-stevedore-impl :pallet.stevedore.batch/batch
    (facts
      (script "42") => "42"
      (script "1/2") => "1/2")))

(deftest simple-call-test
  (with-stevedore-impl :pallet.stevedore.batch/batch
    (facts
      (script (a b c)) => "call:a b c"
      (script (a b)) => "call:a b"
      (script (a)) => "call:a")))

(deftest test-arithmetic
  (with-stevedore-impl :pallet.stevedore.batch/batch
    (fact
      (script (* x y)) => "(x * y)"
      (script (* 1 2)) => "(1 * 2)")))

(deftest test-return
  (with-stevedore-impl :pallet.stevedore.batch/batch
    (future-fact "handle return values from functions"
      ;; http://www.dostips.com/DtTutoFunctions.php
      (script (return 42)) => "return 42")))

(deftest test-set!
  (with-stevedore-impl :pallet.stevedore.batch/batch
    (future-fact "handle arithmatic in set!"
      (script (set! foo (+ 1 1))) => "set /a foo=(1+1)"
      (script (set! foo 1)) => "set /a foo=1")
    (fact "assign simple strings"
      (script (set! foo "1")) => "set foo=1"
      (script (set! foo "1 + 1")) => "set foo=1 + 1"
      (script (set! foo-bar "1")) => (throws clojure.contrib.condition.Condition))))

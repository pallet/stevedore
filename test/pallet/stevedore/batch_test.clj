(ns pallet.stevedore.batch-test
  (:use
   [pallet.common.string :only [quoted]]
   pallet.stevedore
   pallet.stevedore.batch
   clojure.test)
  (:require
   [pallet.stevedore.common :as common]))

(defn with-batch
  [f]
  (with-script-language :pallet.stevedore.batch/batch
    (f)))

(use-fixtures :once with-batch)

;; (deftest implementation-coverage-test
;;   (testing "complete `emit-special` coverage"
;;     (is (empty? (common/emit-special-not-implemented
;;                  :pallet.stevedore.batch/batch)))))

(deftest number-literal
  (is (= (script 42) "42"))
  (is (= (script 1/2) "0.5")))

(deftest test-string
  (is (= (script "42") "42"))
  (is (= (script "1/2") "1/2")))

(deftest simple-call-test
  (is (= (script (a b c)) "call:a b c"))
  (is (= (script (a b)) "call:a b"))
  (is (= (script (a)) "call:a")))

(deftest test-arithmetic
  (is (= (script (* x y)) "(x * y)"))
  (is (= (script (* 1 2)) "(1 * 2)")))

;; (deftest test-return
;;   (testing "handle return values from functions"
;;     ;; http://www.dostips.com/DtTutoFunctions.php
;;     (is (= (script (return 42)) "return 42"))))

(deftest test-set!
  ;; (testing "handle arithmatic in set!"
  ;;   (is (= (script (set! foo (+ 1 1))) "set /a foo=(1+1)"))
  ;;   (is (= (script (set! foo 1)) "set /a foo=1")))
  (testing "assign simple strings"
    (is (= (script (set! foo "1")) "set foo=1"))
    (is (= (script (set! foo "1 + 1")) "set foo=1 + 1"))
    (is (thrown? slingshot.Stone (script (set! foo-bar "1"))))))

(deftest test-str
  (is (= (script (str foo bar)) "foobar")))

(deftest println-test
  (is (= (script (println "hello")) "echo hello"))
  (is (= (script (println "hello there")) "echo hello there")))

(deftest deref-test
  (is (= (script @TMPDIR) "%TMPDIR%"))
  ;; (testing "support default value for defrefencing"
  ;;   (is (= (script @TMPDIR-/tmp) "%TMPDIR%-/tmp")))
  (testing "support equivilant of `ls`"
    (is (script @(ls)) "$(ls)")))

(deftest group-test
  (is (= (script (group (ls))) "(\ncall:ls\n)"))
  (is (= (script (group (ls) (ls))) "(\ncall:ls\ncall:ls\n)")))

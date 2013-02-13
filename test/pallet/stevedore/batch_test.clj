(ns pallet.stevedore.batch-test
  (:require
   [clojure.test :refer [is testing]]
   [pallet.common.string :refer [quoted]]
   [pallet.stevedore :refer :all]
   [pallet.stevedore.batch :refer :all]
   [pallet.stevedore.common]
   [pallet.stevedore.test-common]))

;;; We define a macro rather than a fixture so we can run individual tests
(defmacro deftest [name & body]
  `(clojure.test/deftest ~name
     (with-script-language :pallet.stevedore.batch/batch
       ~@body)))

;; (deftest implementation-coverage-test
;;   (testing "complete `emit-special` coverage"
;;     (is (empty? (common/emit-special-not-implemented
;;                  :pallet.stevedore.batch/batch)))))

(deftest number-literal
  (is (= "42" (script 42)))
  (is (= "0.5" (script 1/2))))

(deftest test-string
  (is (= "42" (script "42")))
  (is (= "1/2" (script "1/2"))))

(deftest simple-call-test
  (is (script= "call:a b c" (script (a b c))))
  (is (script= "call:a b" (script (a b))))
  (is (script= "call:a" (script (a)))))

(deftest test-arithmetic
  (is (script= "(x * y)" (script (* x y))))
  (is (script= "(1 * 2)" (script (* 1 2)))))

;; (deftest test-return
;;   (testing "handle return values from functions"
;;     ;; http://www.dostips.com/DtTutoFunctions.php
;;     (is (= (script (return 42)) "return 42"))))

(deftest test-set!
  ;; (testing "handle arithmatic in set!"
  ;;   (is (= (script (set! foo (+ 1 1))) "set /a foo=(1+1)"))
  ;;   (is (= (script (set! foo 1)) "set /a foo=1")))
  (testing "assign simple strings"
    (is (script= "set foo=1" (script (set! foo "1"))))
    (is (script= "set foo=1 + 1" (script (set! foo "1 + 1"))))
    (is (thrown? clojure.lang.ExceptionInfo (script (set! foo-bar "1"))))))

(deftest test-str
  (is (script= "foobar" (script (str foo bar)))))

(deftest println-test
  (is (script= "echo hello" (script (println "hello"))))
  (is (script= "echo hello there" (script (println "hello there")))))

(deftest deref-test
  (is (script= "%TMPDIR%" (script @TMPDIR)))
  ;; (testing "support default value for defrefencing"
  ;;   (is (= (script @TMPDIR-/tmp) "%TMPDIR%-/tmp")))
  (testing "support equivilant of `ls`"
    (is (script @(ls)) "$(ls)")))

(deftest group-test
  (is (script= "(\ncall:ls\n)" (script (group (ls)))))
  (is (script= "(\ncall:ls\ncall:ls\n)" (script (group (ls) (ls))))))

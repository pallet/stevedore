(ns pallet.stevedore.batch-test
  (:use
   [pallet.common.string :only [quoted]]
   pallet.stevedore
   pallet.stevedore.batch
   [pallet.stevedore.common :only [emit-special-coverage]]
   midje.sweet
   clojure.test))

(deftest implementation-coverage-test
  (fact "complete `emit-special` coverage"
    (let [unimplemented (second (emit-special-coverage :pallet.stevedore.batch/batch))]
      unimplemented => empty?)))

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
    (facts
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

(deftest test-str
  (with-stevedore-impl :pallet.stevedore.batch/batch
    (fact (script (str foo bar)) => "foobar")))

(deftest println-test
  (with-stevedore-impl :pallet.stevedore.batch/batch
    (fact
      (script (println "hello")) => "echo hello"
      (script (println "hello" @world)) => "echo hello %world%"
      (script (println "hello there")) => "echo hello there")))

(deftest deref-test
  (with-stevedore-impl :pallet.stevedore.batch/batch
    (fact
      (script @TMPDIR) => "%TMPDIR%")
    (future-fact "support default value for defrefencing"
      (script @TMPDIR-/tmp) => "%TMPDIR%-/tmp")
    (future-fact "support equivilant of `ls`"
      (script @(ls)) "$(ls)")))

(deftest group-test
  (with-stevedore-impl :pallet.stevedore.batch/batch
    (facts
      (script (group (ls))) => "(\ncall:ls\n)"
      (script (group (ls) (ls))) => "(\ncall:ls\ncall:ls\n)")))

(deftest test-fn
  (with-stevedore-impl :pallet.stevedore.batch/batch
    (fact "Anonymous functions"
          (script (defn [x y]
                    (foo a) (bar b)))
      => (throws java.lang.AssertionError))
    (fact "positional arguments"
      (script (defn foo [x y]
                (foo a)))
      => ":foo\nSETLOCAL\nset x=%~1%\nset y=%~2%\ncall:foo a\nGOTO :EOF")))

(deftest test-if
  (with-stevedore-impl :pallet.stevedore.batch/batch
    (fact "without else"
      (script (if (= "foo" "bar")
                (println "fred")))
      => "if (foo EQU bar) (\necho fred\n)")
    (fact "with else"
      (script (if (= "foo" "bar")
                (println "fred")
                (println "foobar")))
      => "if (foo EQU bar) (\necho fred\n) else (\necho foobar\n)")))

(deftest if-nested-test
  (with-stevedore-impl :pallet.stevedore.batch/batch
    (fact 
      (script (if (== "foo" "bar")
                (if (!= "foo" "baz")
                  (println "fred"))))
      => "if (foo EQU bar) (\nif (foo NEQ baz) (\necho fred\n)\n)")))

(deftest test-when
  (with-stevedore-impl :pallet.stevedore.batch/batch
    (fact
      (script (when (= "foo" "bar") 
                (println "fred")))
      => "if (foo EQU bar) (\necho fred\n\n)")
    (fact 
      (script (when "foo" 
                (var x 3) 
                (foo "x")
                (bar "x")
                (www "x")))
      =>  "if foo (\nset x=3\ncall:foo x\ncall:bar x\ncall:www x\n\n)")))

(deftest test-file-exists
  (with-stevedore-impl :pallet.stevedore.batch/batch
    (fact 
      (script (if (file-exists? "bar")
                (println "fred"))))
      => "if EXIST bar (\necho fred\n)"))

(deftest test-if-not
  (with-stevedore-impl :pallet.stevedore.batch/batch
    (fact 
      (script (if-not (file-exists? "bar") 
                (println "fred"))))
      => "if [ ! -e bar ]; then echo fred;fi"
    (future-fact "if-not and conjunction plays nicely" 
      (script (if-not (and (file-exists? "bar") 
                           (== "foo" "bar")) 
                (println "fred")))
      => "if [ ! \\( -e bar -a \\( \"foo\" EQU \"bar\" \\) \\) ]; then echo fred;fi")))

(deftest text-infix-expr
  (with-stevedore-impl :pallet.stevedore.batch/batch
    (fact 
      (script (if-not (file-exists? "bar") 
                (println "a")))
      =>  "if NOT EXIST bar (\necho a\n)")))


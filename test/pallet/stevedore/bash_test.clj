(ns pallet.stevedore.bash-test
  (:use
   [pallet.common.string :only [quoted]]
   pallet.stevedore
   clojure.test
   pallet.common.slingshot-test-util)
  (:require
   [pallet.script :as script]
   [pallet.stevedore.common]
   [pallet.stevedore.bash]
   [pallet.common.filesystem :as filesystem]
   [pallet.common.logging.logutils :as logutils]
   [pallet.common.shell :as shell]
   [clojure.tools.logging :as logging]))

(defmacro bash-out
  "Check output of bash. Implemented as a macro so that errors appear on the
   correct line."
  ([str] `(bash-out ~str 0 ""))
  ([str exit err-msg]
     `(let [r# (shell/bash ~str)]
        (when-not (= ~exit (:exit r#))
          (logging/errorf
           "Unexpected exit status:\n:cmd %s\n:out %s\n:err %s"
           ~str (:out r#) (:err r#)))
        (is (= ~err-msg (:err r#)))
        (is (= ~exit (:exit r#)))
        (:out r#))))

(defn strip-ws
  "strip extraneous whitespace so tests don't fail because of differences in
   whitespace" [s]
  (-> s
    (.replaceAll "[ ]+" " ")
    .trim))

(defn strip-line-ws
  "strip extraneous whitespace so tests don't fail because of differences in
   whitespace"
  [#^String s]
  (-> s
      (.replace "\n" " ")
      (.replaceAll "[ ]+" " ")
      .trim))

(with-script-language :pallet.stevedore.bash/bash
  (script
    (.dot-method balh "lbha" "alsd")))

(defn with-bash
  [f]
  (with-script-language :pallet.stevedore.bash/bash
    (f)))

(use-fixtures :once with-bash)

(deftest number-literal
  (is (= "42" (script 42)))
  (is (= "0.5" (script 1/2))))

(deftest simple-call-test
  (is (= "a b" (script (a b)))))

(deftest call-multi-arg-test
  (is (= "a b c" (script (a b c)))))

(deftest test-arithmetic
  (is (= "(x * y)" (script (* x y)))))

(deftest test-return
  (is (= "return 42" (script (return 42)))))

(deftest test-script-call
  (let [name "name1"]
    (is (= "grep \"^name1\" /etc/passwd"
           (script (grep ~(str "\"^" name "\"") "/etc/passwd"))))))


(deftest test-clj
  (let [foo 42
        bar [1 2 3]]
    (is (= "42" (script (clj foo))))
    (is (= "42" (script ~foo)))
    (is (= "foo 1 2 3" (script (apply foo ~bar))))))

(deftest test-str
  (is (= "foobar"
         (script (str foo bar)))))

(deftest test-quoted
  (is (= "\"foobar\""
         (script (quoted (str foo bar))))))

(deftest test-fn
  (is (thrown? java.lang.AssertionError
               (strip-ws (script (defn [x y]
                                   (foo a) (bar b)))))
      "anonymous")

  (is (= "foo() {\nx=$1\ny=$2\nfoo a\nbar b\n}"
         (strip-ws (script (defn foo [x y]
                             (foo a) (bar b)))))
      "without flags")

  (is (= "foo() {\nDEFINE_string \"host\" \"default\" \"Doc\" \"h\"\nFLAGS \"$@\" || exit 1\neval set -- \"${FLAGS_ARGV}\"\nfoo a\nbar b\n}"
         (strip-ws (script (defn foo [[:string "host" "h" "Doc" "default"]]
                             (foo a) (bar b)))))
      "with flags only")

  (is (= "foo() {\nDEFINE_string \"host\" \"default\" \"Doc\" \"h\"\nFLAGS \"$@\" || exit 1\neval set -- \"${FLAGS_ARGV}\"\nx=$1\ny=$2\nfoo a\nbar b\n}"
         (strip-ws (script (defn foo [x y
                                      [:string "host" "h" "Doc" "default"]]
                             (foo a) (bar b)))))
      "with flags and arguments")

  (is (= "foo() {\nFLAGS_HELP=\"This is doc\"\nDEFINE_string \"host\" \"default\" \"Doc\" \"h\"\nFLAGS \"$@\" || exit 1\neval set -- \"${FLAGS_ARGV}\"\nx=$1\ny=$2\nfoo a\nbar b\n}"
         (strip-ws (script (defn foo
                             "This is doc"
                             [x y
                              [:string "host" "h" "Doc" "default"]]
                             (foo a) (bar b)))))
      "with docstring and arguments"))


(deftest test-aget
  (is (= "${foo[2]}" (script (aget foo 2)))))


(deftest test-aset
  (is (= "foo[2]=1" (script (aset foo 2 1)))))

(deftest test-set!
  (is (= "foo=1" (script (set! foo 1))))
  (is-thrown-slingshot? (script (set! foo-bar 1))))

(deftest var-test
  (is (= "foo=1" (script (var foo 1))))
  (is-thrown-slingshot? (script (var foo-bar 1))))

(deftest alias-test
  (is (= "alias foo='ls -l'" (script (alias foo (ls -l))))))

(deftest test-array
  (is (= "(1 2 \"3\" foo)" (script [1 "2" "\"3\"" :foo]))))

(deftest test-if
  (is (= "if [ \"foo\" == \"bar\" ]; then echo fred;fi"
         (script (if (= foo bar) (println fred)))))
  (is (= "if [ \"foo\" == \"bar\" ] && [ \"foo\" != \"baz\" ]; then echo fred;fi"
         (script (if (&& (== foo bar) (!= foo baz)) (println fred)))))
  (is (= "fred\n"
         (bash-out (script (if (&& (== foo foo) (!= foo baz)) (println "fred"))))))
  (is (= "if foo; then\nx=3\nfoo x\nelse\ny=4\nbar y\nfi"
         (script (if foo (do (var x 3) (foo x)) (do (var y 4) (bar y))))))
  (is (= "not foo\n"
         (bash-out (script (if (== foo bar)
                             (do (println "foo"))
                             (do (println "not foo")))))))
  (is (= "if [ -e file1 ]; then echo foo;fi"
         (script (if (file-exists? "file1") (println "foo")))))
  (is (= "if ! ( [ -e file1 ] ); then echo foo;fi"
         (script (if (not (file-exists? "file1")) (println "foo")))))
  (is (= "if ! ( [ -e file1 ] ); then echo foo;fi"
         (let [condition (script (file-exists? "file1"))]
           (script (if (not ~condition) (println "foo"))))))
  (is (= "if ! ( [ \"a\" == \"1\" ] && file1 ); then echo foo;fi" ;;; !!!!! checkme
         (let [condition (script (and (= a 1) "file1"))]
           (script (if (not ~condition) (println "foo"))))))
  (is (= "if ! ( grep aa file1 ); then echo foo;fi"
         (script (if (not (grep "aa" "file1")) (println "foo")))))
  (is (= "if ! ( [ -e file1 ] ) || [ \"a\" == \"b\" ]; then echo foo;fi"
         (script (if (|| (not (file-exists? "file1")) (== "a" "b"))
                   (println "foo")))))
  (testing "if block as string with newline is treated as compound"
    (is (= "if [ -e f ]; then\nls\nls\nfi"
           (script (if (file-exists? "f") "ls\nls")))))
  (testing "an expression"
    (is (= "if ! ( [ -e md5 ] ) || ls file; then echo 1;fi"
           (script (if (|| (not (file-exists? "md5"))
                           (ls "file"))
                     (println 1)))))))

(deftest if-nested-test
  (is (= "if [ \"foo\" == \"bar\" ]; then\nif [ \"foo\" != \"baz\" ]; then echo fred;fi\nfi"
         (script (if (== foo bar)
                   (if (!= foo baz)
                     (println fred))))))
  (is (= "" (bash-out (script (if (== foo bar)
                                (if (!= foo baz)
                                  (println fred))))))))

(deftest test-if-not
  (is (= "if ! ( [ -e bar ] ); then echo fred;fi"
         (script (if-not (file-exists? bar) (println fred)))))
  (is (= "if ! ( [ -e bar ] && [ \"foo\" == \"bar\" ] ); then echo fred;fi"
         (script (if-not (&& (file-exists? bar) (== foo bar)) (println fred)))))
  (is (= "if ! ( [ \"foo\" == \"bar\" ] && [ \"foo\" == \"baz\" ] ); then echo fred;fi"
         (script (if-not (&& (== foo bar) (== foo baz)) (println fred)))))
  (is (= "fred\n"
         (bash-out (script (if-not (&& (== foo foo) (== foo baz))
                             (println "fred")))))))

(deftest test-when
  (is (= "if [ \"foo\" == \"bar\" ]; then\necho fred\nfi"
         (script (when (= foo bar) (println fred)))))
  (is (= "if foo; then\nx=3\nfoo x\nfi"
         (script (when foo (var x 3) (foo x))))))

(deftest test-when-not
  (is (= "if ! ( [ \"foo\" == \"bar\" ] ); then\necho fred\nfi"
         (script (when-not  (= foo bar) (println fred)))))
  (is (= "if ! ( foo ); then\nx=3\nfoo x\nfi"
         (script (when-not foo (var x 3) (foo x))))))

(deftest test-case
  (is (= "case ${X} in\n1)\nsomething;;\n\"2\")\nsomething else;;\nesac"
         (script (case @X
                   1 (something)
                   ~(quoted "2") (something else))))))

(deftest test-doseq
  (is (= "for X in 1 2 3; do\nsomething ${X}\ndone"
         (script (doseq [X [1 2 3]] (something @X))))))


(deftest test-map
  (is (= "([packages]=(columnchart))"
         (strip-ws (script {:packages ["columnchart"]}))))
  (is (#{"{ hash_set x p c; hash_set x q d; }\necho ${x[p]}"
         "{ hash_set x q d; hash_set x p c; }\necho ${x[p]}"}
       (strip-ws (script (do (var x {:p "c" :q "d"})
                             (println (aget x :p)))))))
  (is (= "c\nd\n"
         (bash-out (script
                    ~pallet.stevedore.bash/hashlib
                    (var x {:p "c" "/a/b/c-e" "d"})
                    (println (get x :p))
                    (println (get x "/a/b/c-e"))))))
  (testing "assoc!"
    (is (= "c\n1\n2\n"
           (bash-out (script
                      ~pallet.stevedore.bash/hashlib
                      (var x {:p "c" :q "q"})
                      (assoc! x :q 1)
                      (assoc! x :r 2)
                      (println (get x :p))
                      (println (get x :q))
                      (println (get x :r)))))))
  (testing "merge!"
    (is (= "c\n1\n2\n"
           (bash-out (script
                      ~pallet.stevedore.bash/hashlib
                      (var x {:p "c" :q "q"})
                      (merge! x {:q 1 :r 2})
                      (println (get x :p))
                      (println (get x :q))
                      (println (get x :r))))))))


(deftest test-do
  (is (= "let x=3\nlet y=4\nlet z=(x + y)"
         (strip-ws
          (script
           (let x 3)
           (let y 4)
           (let z (+ x y))))))
  (is (= "7\n"
         (bash-out
          (script
           (let x 3)
           (let y 4)
           (let z (+ x y))
           (println @z))))))

(deftest deref-test
  (is (= "${TMPDIR-/tmp}" (script @TMPDIR-/tmp)))
  (is (= "$(ls)" (script @(ls)))))

(deftest test-combine-forms
  (let [stuff (quote (do
                       (local x 3)
                       (local y 4)))]
    (is (= "foo() {\nx=$1\nlocal x=3\nlocal y=4\n}"
           (strip-ws (script (defn foo [x] ~stuff)))))))

(deftest defvar-test
  (is (= "x=1"
         (script (defvar x 1)))))

(deftest println-test
  (is (= "echo hello"
         (script (println "hello"))))
  (is (= "echo hello there"
         (script (println "hello there")))))

(deftest do-script-test
  (is (= "fred\n" (do-script "fred")))
  (is (= "fred\nblogs\n" (do-script "fred" "blogs")))
  (is (= "fred\nblogs\n" (do-script "fred\n\n" "blogs\n")))
  (is (= "fred\nblogs\n" (do-script "fred\n\n" nil "blogs\n"))))

(deftest chain-commands-test
  (is (= "fred" (apply chain-commands ["fred"])))
  (is (= "fred && blogs" (apply chain-commands ["fred" "blogs"])))
  (is (= "fred && blogs" (apply chain-commands ["fred\n\n" "blogs\n"])))
  (is (= "fred && blogs" (apply chain-commands ["fred\n\n" nil "blogs\n"])))
  (is (= "fred" (chain-commands "fred")))
  (is (= "fred && blogs" (chain-commands "fred" "blogs")))
  (is (= "fred && blogs" (chain-commands "fred\n\n" "blogs\n")))
  (is (= "fred && blogs" (chain-commands "fred\n\n" nil "blogs\n"))))

(deftest chain-script-test
  (is (= "fred" (chained-script (fred))))
  (is (= "fred && blogs" (chained-script (fred) (blogs)))))

(deftest checked-commands-test
  (is (= "echo \"test...\"\n{ echo fred && echo tom; } || { echo \"test\" failed; exit 1; } >&2 \necho \"...done\"\n"
         (checked-commands "test" "echo fred" "echo tom")))
  (is (= "test...\ntom\n...done\n"
         (bash-out (checked-commands "test" "echo tom"))))
  (is (= "test...\nfred\ntom\n...done\n"
         (bash-out (checked-commands "test" "echo fred" "echo tom"))))
  (is (= "test...\n"
         (bash-out
          (checked-commands "test" "test 1 = 2") 1 "test failed\n"))))

(deftest checked-script-test
  (is (= (checked-commands "msg" (script ls) (script ls))
         (checked-script "msg" (ls) (ls))))
  (is (= "echo \"test...\"\n{ echo fred && echo tom; } || { echo \"test\" failed; exit 1; } >&2 \necho \"...done\"\n"
         (checked-script "test" (println fred) (println tom))))
  (is (= "test...\ntom\n...done\n"
         (bash-out (checked-script "test" (println tom)))))
  (is (= "test...\nfred\ntom\n...done\n"
         (bash-out (checked-script "test" (println fred) (println tom)))))
  (is (= "test...\n"
         (bash-out (checked-script "test" ("test" 1 = 2)) 1 "test failed\n"))))

(deftest group-test
  (is (= "{ ls; }"
         (script (group (ls)))))
  (is (= "{ ls; ls; }"
         (script (group (ls) (ls))))))

(deftest pipe-test
  (is (= "ls"
         (script (pipe (ls)))))
  (is (= "ls | ls"
         (script (pipe (ls) (ls))))))

(deftest empty?-test
  (is (= "if [ -z ${a} ]; then echo true;fi"
         (script (if (empty? @a) (println true))))))

(deftest unquote-splicing-test
  (is (= "a b c" (script ~@["a" "b" "c"])))
  (is (= "x" (script x ~@[])))
  (is (= "x" (script (x ~@[]))))
  (let [x ["a" "b" "c"]]
    (is (= "a b c" (script ~@x))))
  (let [x []]
    (is (= "x" (script x ~@x))))
  (let [x nil]
    (is (= "" (script ~@x))))
  (let [x []]
    (is (= "" (script ~@x))))
  (let [fx (fn [] ["a" "b" "c"])]
    (is (= "a b c" (script ~@(fx)))))
  (let [xfn (script/script-fn [& args])]
    (script/defimpl xfn :default [& args]
      ("xfn" ~args))
    (let [x nil]
      (is (= "xfn" (script (xfn ~@x)))))
    (let [x [:a 1]]
      (is (= "xfn a 1" (script (xfn ~@x)))))))

(logutils/with-threshold [:error]
  (script/defscript x [a])
  (defimpl x :default [a] a))

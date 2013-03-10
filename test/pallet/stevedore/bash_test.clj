(ns pallet.stevedore.bash-test
  (:require
   [clojure.string :as string]
   [clojure.test :refer [is testing]]
   [clojure.tools.logging :as logging]
   [pallet.common.filesystem :as filesystem]
   [pallet.common.logging.logutils :as logutils]
   [pallet.common.shell :as shell]
   [pallet.common.string :refer [quoted]]
   [pallet.script :as script]
   [pallet.stevedore :refer :all]
   [pallet.stevedore.bash :refer :all]
   [pallet.stevedore.common]
   [pallet.stevedore.test-common]))

(defmacro current-line [] (-> &form meta :line))

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
        (logging/tracef "bash-out %s %s" ~str r#)
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

;;; We define a macro rather than a fixture so we can run individual tests
(defmacro deftest [name & body]
  `(clojure.test/deftest ~name
     (with-script-language :pallet.stevedore.bash/bash
       ~@body)))

(deftest number-literal
  (is (= "42" (script 42)))
  (is (= "0.5" (script 1/2))))

(deftest no-comment-on-empty
  (is (= "" (script ""))))

(deftest simple-call-test
  (is (script= "a b" (script (a b))))
  (is (= "a b" (with-source-line-comments nil (script (a b)))))
  (is (= (str "    # bash_test.clj:" (current-line) "\na b") (script (a b)))
      "has source comment on first symbol only (not on args)"))

(deftest call-multi-arg-test
  (is (script= "a b c" (script (a b c)))))

(deftest test-arithmetic
  (is (script= "(x * y)" (script (* x y)))))

(deftest test-return
  (is (script= "return 42" (script (return 42)))))

(deftest test-script-call
  (let [name "name1"]
    (is (script= "grep \"^name1\" /etc/passwd"
           (script (grep ~(str "\"^" name "\"") "/etc/passwd"))))))


(deftest test-clj
  (let [foo 42
        bar [1 2 3]]
    ;; (is (= "42" (script (clj foo))))
    (is (= "42" (script ~foo)))
    (is (script= "foo 1 2 3" (script (apply foo ~bar))))))

(deftest test-str
  (is (script= "foobar"
         (script (str foo bar)))))

(deftest test-quoted
  (is (script= "\"foobar\""
               (script (quoted (str foo bar))))))

(deftest test-fn
  (is (thrown? java.lang.AssertionError
               (strip-ws (script (defn [x y]
                                   (foo a) (bar b)))))
      "anonymous")

  (is (script=
       "foo() {\nx=$1\ny=$2\nfoo a\nbar b\n}"
       (strip-ws (script (defn foo [x y] (foo a) (bar b)))))
      "without flags")

  (is (script=
       (str "foo() {\nDEFINE_string \"host\" \"default\" \"Doc\" \"h\"\n"
            "FLAGS \"$@\" || exit 1\n"
            "eval set -- \"${FLAGS_ARGV}\"\nfoo a\nbar b\n}")
       (strip-ws (script (defn foo [[:string "host" "h" "Doc" "default"]]
                           (foo a) (bar b)))))
      "with flags only")

  (is (script=
       (str "foo() {\nDEFINE_string \"host\" \"default\" \"Doc\" \"h\"\n"
            "FLAGS \"$@\" || exit 1\n"
            "eval set -- \"${FLAGS_ARGV}\"\nx=$1\ny=$2\nfoo a\nbar b\n}")
       (strip-ws (script (defn foo [x y
                                    [:string "host" "h" "Doc" "default"]]
                           (foo a) (bar b)))))
      "with flags and arguments")

  (is (script=
       (str "foo() {\nFLAGS_HELP=\"This is doc\"\nDEFINE_string \"host\" "
            "\"default\" \"Doc\" \"h\"\nFLAGS \"$@\" || exit 1\neval set -- "
            "\"${FLAGS_ARGV}\"\nx=$1\ny=$2\nfoo a\nbar b\n}")
       (strip-ws (script (defn foo
                           "This is doc"
                           [x y
                            [:string "host" "h" "Doc" "default"]]
                           (foo a) (bar b)))))
      "with docstring and arguments"))


(deftest test-aget
  (is (script= "${foo[2]}" (script (aget foo 2)))))


(deftest test-aset
  (is (script= "foo[2]=1" (script (aset foo 2 1)))))

(deftest test-set!
  (is (script= "foo=1" (script (set! foo 1))))
  (is (thrown? clojure.lang.ExceptionInfo (script (set! foo-bar 1)))))

(deftest var-test
  (is (script= "foo=1" (script (var foo 1))))
  (is (thrown? clojure.lang.ExceptionInfo (script (var foo-bar 1)))))

(deftest alias-test
  (is (script= "alias foo='ls -l'" (script (alias foo (ls -l))))))

(deftest test-array
  (is (script= "(1 2 \"3\" foo)" (script [1 "2" "\"3\"" :foo]))))

(deftest test-if
  (is (script= "if [ \"foo\" == \"bar\" ]; then echo fred;fi"
               (script (if (= foo bar) (println fred)))))
  (is (script=
       "if [ \"foo\" == \"bar\" ] && [ \"foo\" != \"baz\" ]; then echo fred;fi"
       (script (if (&& (== foo bar) (!= foo baz)) (println fred)))))
  (is (= "fred\n"
         (bash-out
          (script (if (&& (== foo foo) (!= foo baz)) (println "fred"))))))
  (is (script= "if foo; then\nx=3\nfoo x\nelse\ny=4\nbar y\nfi"
               (script (if foo (do (var x 3) (foo x)) (do (var y 4) (bar y))))))
  (is (= "not foo\n"
         (bash-out (script (if (== foo bar)
                             (do (println "foo"))
                             (do (println "not foo")))))))
  (is (script= "if [ -e file1 ]; then echo foo;fi"
               (script (if (file-exists? "file1") (println "foo")))))
  (is (script= "if ! ( [ -e file1 ] ); then echo foo;fi"
               (script (if (not (file-exists? "file1")) (println "foo")))))
  (is (= "foo\n"
         (bash-out
          (script (if (not (file-exists? "file1")) (println "foo"))))))
  (is (script= "if ! ([ -e file1 ] ); then echo foo;fi"
               (let [condition (script (file-exists? "file1"))]
                 (script (if (not ~condition) (println "foo"))))))
  (is (= "foo\n"
         (bash-out (let [condition (script (file-exists? "file1"))]
                     (script (if (not ~condition) (println "foo")))))))
  (is (script=
       (str "if ! ([ \"a\" == \"1\" ] && file1 ); then echo foo;fi")
       (let [condition (script (and (= a 1) "file1"))]
         (script (if (not ~condition) (println "foo"))))))
  (is (script= "if ! ( grep aa file1 ); then echo foo;fi"
               (script (if (not (grep "aa" "file1")) (println "foo")))))
  (is (script= "if ! ( [ -e file1 ] ) || [ \"a\" == \"b\" ]; then echo foo;fi"
               (script (if (|| (not (file-exists? "file1")) (== "a" "b"))
                         (println "foo")))))
  (testing "if block as string with newline is treated as compound"
    (is (script= "if [ -e f ]; then\nls\nls\nfi"
                 (script (if (file-exists? "f") "ls\nls")))))
  (testing "an expression"
    (is (script= "if ! ( [ -e md5 ] ) || ls file; then echo 1;fi"
                 (script (if (|| (not (file-exists? "md5"))
                                 (ls "file"))
                           (println 1)))))))

(deftest if-nested-test
  (is (script=
       (str "if [ \"foo\" == \"bar\" ]; then\nif [ \"foo\" != \"baz\" ]; "
            "then echo fred;fi\nfi")
       (script (if (== foo bar)
                 (if (!= foo baz)
                   (println fred))))))
  (is (= "" (bash-out (script (if (== foo bar)
                                (if (!= foo baz)
                                  (println fred))))))))

(deftest test-if-not
  (is (script=
       "if ! ( [ -e bar ] ); then echo fred;fi"
       (script (if-not (file-exists? bar) (println fred)))))
  (is (script=
       "if ! ( [ -e bar ] && [ \"foo\" == \"bar\" ] ); then echo fred;fi"
       (script (if-not (&& (file-exists? bar) (== foo bar)) (println fred)))))
  (is (script=
       (str "if ! ( [ \"foo\" == \"bar\" ] && [ \"foo\" == \"baz\" ] ); "
            "then echo fred;fi")
       (script (if-not (&& (== foo bar) (== foo baz)) (println fred)))))
  (is (= "fred\n"
         (bash-out (script (if-not (&& (== foo foo) (== foo baz))
                             (println "fred")))))))

(deftest test-when
  (is (script= "if [ \"foo\" == \"bar\" ]; then\necho fred\nfi"
               (script (when (= foo bar) (println fred)))))
  (is (script= "if foo; then\nx=3\nfoo x\nfi"
               (script (when foo (var x 3) (foo x))))))

(deftest test-when-not
  (is (script= "if ! ( [ \"foo\" == \"bar\" ] ); then\necho fred\nfi"
               (script (when-not  (= foo bar) (println fred)))))
  (is (script= "if ! ( foo ); then\nx=3\nfoo x\nfi"
               (script (when-not foo (var x 3) (foo x))))))

(deftest test-case
  (is (script= "case ${X} in\n1)\nsomething;;\n\"2\")\nsomething else;;\nesac"
               (script (case @X
                         1 (something)
                         ~(quoted "2") (something else))))))

(deftest test-doseq
  (is (script= "for X in 1 2 3; do\nsomething ${X}\ndone"
               (script (doseq [X [1 2 3]] (something @X)))))
  (is (script= "for X in $(ls); do\nsomething ${X}\ndone"
               (script (doseq [X @(ls)] (something @X))))))


(deftest test-map
  (is (script= "([packages]=(columnchart))"
               (strip-ws (script {:packages ["columnchart"]}))))
  (is (script= "{ hash_set x q d; hash_set x p c;  }\necho ${x[p]}"
       (script (do (var x {:p "c" :q "d"})
                   (println (aget x :p))))))
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
  (is (script= "let x=3\nlet y=4\nlet z=(x + y)"
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
  (is (script= "${TMPDIR-/tmp}" (script @TMPDIR-/tmp)))
  (is (script= "$(ls)" (script @(ls))))
  (is (bash-out (checked-commands "ls"))))

(deftest test-combine-forms
  (let [stuff  `(do
                  (local ~'x 3)
                  (local ~'y 4))]
    (is (script= "foo() {\nx=$1\nlocal x=3\nlocal y=4\n}"
                 (script (defn foo [x] ~stuff))))))

(deftest defvar-test
  (is (script= "x=1"
         (script (defvar x 1)))))

(deftest println-test
  (is (script= "echo hello"
         (script (println "hello"))))
  (is (script= "echo hello there"
         (script (println "hello there")))))

(deftest do-script-test
  (is (script= "fred" (do-script "fred")))
  (is (script= "fred\nblogs" (do-script "fred" "blogs")))
  (is (script= "fred\nblogs" (do-script "fred\n\n" "blogs\n")))
  (is (script= "fred\nblogs" (do-script "fred\n\n" nil "blogs\n"))))

(deftest chain-commands-test
  (is (script= "fred" (apply chain-commands ["fred"])))
  (is (script= "fred && \\\nblogs" (apply chain-commands ["fred" "blogs"])))
  (is (script= "fred && \\\nblogs"
               (apply chain-commands ["fred\n\n" "blogs\n"])))
  (is (script= "fred && \\\nblogs"
               (apply chain-commands ["fred\n\n" nil "blogs\n"])))
  (is (script= "fred" (chain-commands "fred")))
  (is (script= "fred && \\\nblogs" (chain-commands "fred" "blogs")))
  (is (script= "fred && \\\nblogs" (chain-commands "fred\n\n" "blogs\n")))
  (is (script= "fred && \\\nblogs"
               (chain-commands "fred\n\n" nil "blogs\n"))))

(deftest chain-script-test
  (is (script= "fred" (chained-script (fred))))
  (is (script= "fred && \\\nblogs" (chained-script (fred) (blogs)))))

(deftest checked-commands-test
  (is (script=
       (str "echo 'test...';\n{\necho fred && \\\necho tom\n } || "
            "{ echo '#> test : FAIL'; exit 1;} >&2 "
            "\necho '#> test : SUCCESS'")
       (checked-commands "test" "echo fred" "echo tom")))
  (is (= "test...\ntom\n#> test : SUCCESS\n"
         (bash-out (checked-commands "test" "echo tom"))))
  (is (= "test...\nfred\ntom\n#> test : SUCCESS\n"
         (bash-out (checked-commands "test" "echo fred" "echo tom"))))
  (is (= "test...\n"
         (bash-out
          (checked-commands "test" "test 1 = 2") 1 "#> test : FAIL\n"))))

(deftest checked-script-test
  (is (script-no-ws=
       (checked-commands "msg" (script ls) (script ls))
       (checked-script "msg" (ls) (ls))))
  (is (script=
       (str "echo 'test...';\n{\necho fred && \\\necho tom\n } || "
            "{ echo '#> test : FAIL'; exit 1;} >&2 "
            "\necho '#> test : SUCCESS'")
       (checked-script "test" (println fred) (println tom))))
  (is (= "test...\ntom\n#> test : SUCCESS\n"
         (bash-out (checked-script "test" (println tom)))))
  (is (= "test...\nfred\ntom\n#> test : SUCCESS\n"
         (bash-out (checked-script "test" (println fred) (println tom)))))
  (is (= "test...\n"
         (bash-out
          (checked-script "test" ("test" 1 = 2)) 1 "#> test : FAIL\n"))))

(deftest group-test
  (is (script= "{ ls; }"
               (script (group (ls)))))
  (is (script= "{ ls; ls; }"
               (script (group (ls) (ls))))))

(deftest pipe-test
  (is (script= "ls"
               (script (pipe (ls)))))
  (is (script= "ls | \\\nls"
               (script (pipe (ls) (ls)))))
  (is (= "2"
         (string/trim (bash-out
                       (script (pipe (echo "one two") (wc -w))))))))

(deftest empty?-test
  (is (script= "if [ -z ${a} ]; then echo true;fi"
               (script (if (empty? @a) (println true))))))

(deftest unquote-splicing-test
  (is (script= "a b c" (script ~@["a" "b" "c"])))
  (is (script= "x" (script x ~@[])))
  (is (script= "x" (script ("x" ~@[]))))
  (is (script= "x" (script ("x" ~@(list)))))
  (let [x ["a" "b" "c"]]
    (is (script= "a b c" (script ~@x))))
  (let [x []]
    (is (script= "x" (script x ~@x))))
  (let [x nil]
    (is (script= "" (script ~@x)))
    (is (script= "a" (script (str "a" ~@x)))))
  (let [x []]
    (is (script= "" (script ~@x))))
  (let [fx (fn [] ["a" "b" "c"])]
    (is (script= "a b c" (script ~@(fx))))
    (is (script= "abc" (script (str ~@(fx))))))
  (let [xfn (script/script-fn [& args])]
    (script/defimpl xfn :default [& args]
      ("xfn" ~@args))
    (let [x nil]
      (is (script= "xfn" (script (xfn ~@x)))))
    (let [x [:a 1]]
      (is (script= "xfn a 1" (script (xfn ~@x)))))))

(logutils/with-threshold [:error]
  (script/defscript x [a])
  (script/defimpl x :default [a] a))

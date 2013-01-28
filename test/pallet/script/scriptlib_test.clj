(ns pallet.script.scriptlib-test
  (:require
   [pallet.stevedore :as stevedore]
   [pallet.stevedore.test-common])
  (:use pallet.script.scriptlib
        clojure.test
        pallet.script))

(comment
(spit "test.sh"
  (with-script-context [:default]
    (stevedore/script
      ". \"/home/ambrose/.cljr/bin/shflags\""
      (~declare-arguments
         "Test"
         [movie
          [:integer days d "Number of days in movie" 1]
          [:string subtitle s "Subtitle of the movie" "Dooomm"]])

      (echo @movie @days @subtitle)
      (defn foo
        [a]
        (echo @a)))))
)


(deftest test-declare-arguments
  (stevedore/with-script-language :pallet.stevedore.bash/bash
    (is (script=
         "FLAGS_HELP=\"Test\"\nDEFINE_integer \"asdf\" \"e\" \"asdf\" \"a\"\nFLAGS \"$@\" || exit 1\neval set -- \"${FLAGS_ARGV}\"\na=$1\nb=$2\nc=$3"
         (with-script-context [:default]
           (stevedore/script
            (~declare-arguments
             "Test"
             [a b c
              [:integer asdf a "asdf" "e"]]))))
        "With docstring")
    (is (script=
         "DEFINE_string \"asdf\" \"e\" \"asdf\" \"a\"\nFLAGS \"$@\" || exit 1\neval set -- \"${FLAGS_ARGV}\""
         (with-script-context [:default]
           (stevedore/script
            (~declare-arguments
             [[:string asdf a "asdf" "e"]]))))
        "Only flags")
    (is (script=
         "a=$1\nb=$2\nc=$3"
         (with-script-context [:default]
           (stevedore/script
            (~declare-arguments
             [a b c]))))
        "Only args")
    (is (script=
         "DEFINE_string \"asdf\" \"e\" \"asdf\" \"a\"\nFLAGS \"$@\" || exit 1\neval set -- \"${FLAGS_ARGV}\"\na=$1\nb=$2\nc=$3"
         (with-script-context [:default]
           (stevedore/script
            (~declare-arguments
             [a b c
              [:string asdf a "asdf" "e"]]))))
        "Without docstring")
    (is (script=
         ""
         (with-script-context [:default]
           (stevedore/script
            (~declare-arguments
             []))))
        "Empty args")))

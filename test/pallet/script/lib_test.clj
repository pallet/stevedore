(ns pallet.script.lib-test
  (:require [pallet.stevedore :as stevedore])
  (:use pallet.script.lib
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
  (stevedore/with-stevedore-impl :bash
    (is (= "FLAGS_HELP=\"Test\"\nDEFINE_integer \"asdf\" \"e\" \"asdf\" \"a\"\nFLAGS \"$@\" || exit 1\neval set -- \"${FLAGS_ARGV}\"\na=$1\nb=$2\nc=$3\n"
           (with-script-context [:default]
                                (stevedore/script
                                  (~declare-arguments
                                     "Test"
                                     [a b c
                                      [:integer asdf a "asdf" "e"]]))))
        "With docstring")
    (is (= "DEFINE_string \"asdf\" \"e\" \"asdf\" \"a\"\nFLAGS \"$@\" || exit 1\neval set -- \"${FLAGS_ARGV}\"\n"
           (with-script-context [:default]
                                (stevedore/script
                                  (~declare-arguments
                                     [[:string asdf a "asdf" "e"]]))))
        "Only flags")
    (is (=  "FLAGS \"$@\" || exit 1\neval set -- \"${FLAGS_ARGV}\"\na=$1\nb=$2\nc=$3\n"
           (with-script-context [:default]
                                (stevedore/script
                                  (~declare-arguments
                                     [a b c]))))
        "Only args")
    (is (= "DEFINE_string \"asdf\" \"e\" \"asdf\" \"a\"\nFLAGS \"$@\" || exit 1\neval set -- \"${FLAGS_ARGV}\"\na=$1\nb=$2\nc=$3\n"
           (with-script-context [:default]
                                (stevedore/script
                                  (~declare-arguments
                                     [a b c
                                      [:string asdf a "asdf" "e"]]))))
        "Without docstring")
    (is (= "FLAGS \"$@\" || exit 1\neval set -- \"${FLAGS_ARGV}\"\n"
           (with-script-context [:default]
                                (stevedore/script
                                  (~declare-arguments
                                     []))))
        "Empty args")))

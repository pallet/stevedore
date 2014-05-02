(ns pallet.stevedore.common-test
  (:require
   [clojure.test :refer :all]
   [pallet.stevedore :refer [checked-commands with-script-language]]
   [pallet.stevedore.common :refer :all]))

(deftest chain-with-test
  (is (= "a" (chain-with "&&" ["a"])) "no indent")
  (is (= "# abc.clj:123\na" (chain-with "&&" ["# abc.clj:123\na"])) "no indent")
  (is (= "# abc.clj:123\na && \\\n# abc.clj:123\nb"
         (chain-with "&&" ["# abc.clj:123\na" "# abc.clj:123\nb"])))
  (is (= "# a.clj:1  # a.clj:2\na && \\\n# a.clj:3  # a.clj:4\nb"
         (chain-with "&&" ["# a.clj:1  # a.clj:2\na"
                           "# a.clj:3  # a.clj:4\nb"]))))

(deftest checked-commands-test
  (with-script-language :pallet.stevedore.common/common-impl
    (is (.startsWith (checked-commands "abc" "ls")
                     "echo 'abc...'"))
    (is (.startsWith (checked-commands "ab'c" "ls")
                     "echo 'ab'\\''c...'"))))

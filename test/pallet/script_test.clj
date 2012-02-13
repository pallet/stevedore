(ns pallet.script-test
  (:use
   [pallet.stevedore :only [with-script-language]]
   pallet.script
   clojure.test
   pallet.common.slingshot-test-util))

(deftest matches?-test
  (with-script-context [:ubuntu]
    (is (#'pallet.script/matches? [:ubuntu]))
    (is (not (#'pallet.script/matches? [:fedora])))
    (is (not (#'pallet.script/matches? [:ubuntu :smallest]))))
  (with-script-context [:ubuntu :smallest]
    (is (#'pallet.script/matches? [:ubuntu]))
    (is (#'pallet.script/matches? [:smallest]))
    (is (not (#'pallet.script/matches? [:fedora])))
    (is (#'pallet.script/matches? [:ubuntu :smallest]))))

(deftest more-explicit?-test
  (is (#'pallet.script/more-explicit? :default [:anything]))
  (is (#'pallet.script/more-explicit? [:something] [:anything :longer]))
  (is (not (#'pallet.script/more-explicit? [:something :longer] [:anything]))))

(deftest script-fn-test
  (testing "no varargs"
    (let [f (script-fn [a b])]
      (is (= :anonymous (:fn-name f)))
      (with-script-context [:a]
        (is-thrown-slingshot? (dispatch f [1 1]))
        (implement f :default (fn [a b] b))
        (is (= 2 (dispatch f [1 2]))))))
  (testing "varargs"
    (let [f (script-fn [a b & c])]
      (with-script-context [:a]
        (is-thrown-slingshot? (dispatch f [1 1 2 3]))
        (implement f :default (fn [a b & c] c))
        (is (= [2 3] (dispatch f [1 1 2 3]))))))
  (testing "named"
    (let [f (script-fn :fn1 [a b])]
      (is (= :fn1 (:fn-name f))))))

(deftest best-match-test
  (let [s (script-fn [])
        f1 (fn [] 1)
        f2 (fn [] 2)]
    (implement s :default f1)
    (implement s [:os-x] f2)
    (with-script-context [:centos :yum]
      (is (= f1 (#'pallet.script/best-match @(:methods s))))
      (is (= 1 (invoke s []))))
    (with-script-context [:os-x :brew]
      (is (= f2 (#'pallet.script/best-match @(:methods s))))
      (is (= 2 (invoke s []))))))

(deftest defscript-test
  (with-script-context [:a]
    (testing "no varargs"
      (defscript script1a [a b])
      (is (nil? (:doc (meta script1a))))
      (is (= '([a b]) (:arglists (meta #'script1a))))
      (implement script1a :default (fn [a b] b))
      (is (= 2 (dispatch script1a [1 2]))))
    (testing "varargs"
      (defscript script2 "doc" [a b & c])
      (is (= "doc" (:doc (meta #'script2))))
      (is (= '([a b & c]) (:arglists (meta #'script2))))
      (implement script2 :default (fn [a b & c] c))
      (is (= [2 3] (dispatch script2 [1 1 2 3]))))))

(deftest dispatch-test
  (with-script-language :pallet.stevedore.bash/bash
    (let [x (script-fn [a])]
      (testing "with no implementation"
        (testing "should raise"
          (pallet.stevedore/with-script-fn-dispatch
            script-fn-dispatch
            (with-script-context [:ubuntu]
              (is-thrown-slingshot? (pallet.stevedore/script (~x 2)))))))
      (testing "with an implementation"
        (defimpl x :default [a] (str "x" ~a 1))
        (testing "and mandatory dispatch"
          (pallet.stevedore/with-script-fn-dispatch
            script-fn-dispatch
            (with-script-context [:ubuntu]
              (is (= "x21" (pallet.stevedore/script (~x 2)))))))))))

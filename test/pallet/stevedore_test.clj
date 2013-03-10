(ns pallet.stevedore-test
  (:require
   [clojure.test :refer :all]
   [pallet.script :refer [defscript]]
   [pallet.stevedore :refer :all]))

(defmacro line [] (:line (meta &form)))
(defmacro file [] *file*)

(deftest quasiquote-test
  (binding [*apply-form-meta* false]
    (is (= `[(list '~'var '~'a '~'b)] (quasiquote* '[(var a b)])))
    (is (= `((list '~'var ~'a '~'b)) (let [a 1] (quasiquote* '[(var ~a b)]))))
    (is (= `((list '~'a 1)) (quasiquote* '[(a 1)])))))

(deftest metadata-test
  (testing "metadata"
    (is (= (line) (:line (meta (quasiquote (var a b))))))
    (is (= (line) (:line (meta (first (quasiquote [(var a b)]))))))))

(deftest quasiquote-symboltest
  (binding [*apply-form-meta* false]
    (is (= `((list '~'defn ['~'a] '~'a)) (quasiquote* '[(defn [a] a)]))))
  (is (= (line) (:line (meta (quasiquote (defn [a] a)))))))

(deftest quasiquote-unquote-test
  (binding [*apply-form-meta* false]
    (with-redefs [resolve-script-fns false]
      (let [a (fn [x] (str "a" x))]
        (testing "unquote resolves first symbol"
          (is (= `((list ~'a 1))
                 (quasiquote* '[(~a 1)]))))
        (testing "first symbol is quoted"
            (is (= `((list '~'a 1))
                   (quasiquote* '[(a 1)]))))))))

(defscript test-script [x])

(deftest quasiquote-resolve-test
  (binding [*apply-form-meta* false]
    (with-redefs [resolve-script-fns true]
      (with-script-language ::x
        (testing "first symbol is resolved"
          (let [a (fn [x] (str "a" x))]
            (is (= `((list ~'a 1))
                   (quasiquote* '[(a 1)])))))
        (testing "qualified symbol is resolved"
          (let [a (fn [x] (str "a" x))]
            (is (= `((list ~'pallet.stevedore-test/test-script 1))
                   (quasiquote* '[(pallet.stevedore-test/test-script 1)])))))
        (testing "special-forms are quoted"
          (is (= `((list '~'str 1))
                 (quasiquote* '[(str 1)]))))))))

(deftest splice-args-test
  (is (= ["a" "b" "c"]
         (splice-args ["a" "b" "c"])))
  (is (= ["a" "b" "c"]
         (splice-args ["a" '(:pallet.stevedore/splice ["b" "c"])]))))

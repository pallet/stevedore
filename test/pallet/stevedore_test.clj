(ns pallet.stevedore-test
  (:use
   clojure.test
   pallet.stevedore))

(defmacro current-line [] (:line (meta &form)))

(deftest quasiquote-test
  (is (= '((var a b)) (quasiquote [(var a b)])))
  (is (= '((var 1 b)) (let [a 1] (quasiquote [(var ~a b)])))))

(deftest metadata-test
  (testing "metadata"
    (is (= (current-line) (:line (meta (quasiquote (var a b))))))
    (is (= (current-line) (:line (meta (first (quasiquote [(var a b)]))))))))

(deftest quasiquote-symboltest
  (is (= '((defn [a] a)) (quasiquote [(defn [a] a)])))
  (is (= (current-line) (:line (meta (quasiquote (defn [a] a)))))))

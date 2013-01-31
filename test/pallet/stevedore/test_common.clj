(ns pallet.stevedore.test-common
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.test :refer :all]))

(defn source-comment-re-str []
  (str "(?sm) *# " (.getName (io/file *file*)) ":\\d+\n?"))

;;; a test method that adds a check for source line comment
(defmethod assert-expr 'script= [msg form]
  (let [[_ expected expr] form]
    `(let [re# (re-pattern ~(source-comment-re-str))
           expected# (-> ~expected string/trim)
           actual# (-> ~expr (string/replace re# "") string/trim)]
       (if (= expected# actual#)
         (do-report
          {:type :pass :message ~msg :expected expected# :actual actual#})
         (do-report
          {:type :fail :message ~msg :expected expected# :actual actual#})))))

(defmethod assert-expr 'script-no-ws= [msg form]
  (let [[_ expected expr] form]
    `(let [re# (re-pattern ~(source-comment-re-str))
           expected# (-> ~expected
                         string/trim
                         (string/replace #" +" ""))
           actual#  (-> ~expr
                        (string/replace re# "")
                        string/trim
                        (string/replace #" +" ""))]
       (if (= expected# actual#)
         (do-report
          {:type :pass :message ~msg :expected expected# :actual actual#})
         (do-report
          {:type :fail :message ~msg :expected expected# :actual actual#})))))

(ns pallet.script.lib
  "Script library for abstracting target host script differences"
  (:require
    [pallet.script :as script]
    [pallet.stevedore :as stevedore]
    [clojure.string :as string]))

(script/defscript declare-arguments [& args])
(script/defimpl declare-arguments :default
  [& args]
  ~(let [[doc? sig] (if (= (count args) 2)
                      args
                      [nil (first args)])]
     (stevedore/shflags-make-declaration doc? sig)))

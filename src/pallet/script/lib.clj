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

(script/defscript java
  "Command for the java client"
  [& {:keys [main-class jar args classpath server]}])
(script/defimpl java :default 
  [& {:keys [main-class jar args classpath server client] :or {main-class "" args ""}}]
  (java
    ~(stevedore/map-to-arg-string {:client client
                         :server server
                         :classpath (apply str (interpose ":" (map stevedore/emit classpath)))
                         :jar jar}
       :dash "-")
    ~main-class ~args))


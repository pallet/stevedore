(defproject com.palletops/stevedore "0.8.0-SNAPSHOT"
  :description "Embeds shell script in clojure"
  :url "http://palletops.com"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :scm {:url "git@github.com:pallet/stevedore.git"}

  :dependencies [[org.clojure/tools.logging "0.2.0"
                  :exclusions [org.clojure/clojure]]
                 [com.palletops/pallet-common "0.4.0"
                  :exclusions [org.clojure/clojure]]])

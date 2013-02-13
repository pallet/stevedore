(defproject com.palletops/stevedore "0.8.0-beta.1"
  :description "Embeds shell script in clojure"
  :url "http://palletops.com"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :scm {:url "git@github.com:pallet/stevedore.git"}

  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/tools.logging "0.2.0"]
                 [org.cloudhoist/pallet-common "0.3.1"]]
  :profiles
  {:dev {:dependencies [[ch.qos.logback/logback-classic "1.0.9"]]}})

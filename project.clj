(defproject viv "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :jvm-opts ["-Dapple.awt.UIElement=true" "-Djava.library.path=/usr/local/lib/viv"]
  :plugins [[lein-midje "3.0.0"]]
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [clojure-lanterna "0.9.4"]]
  :profiles {:dev {:dependencies [[midje "1.5.1"]]}
             :uberjar {:aot :all}}
  :aliases {"install" ^{:doc "Install viv on this machine"}
            ["do" "uberjar" ["run" "-m" "viv.install/install"]]}
  :java-source-paths ["java-src"]
  :main viv.core)

(defproject avi "0.1.0-SNAPSHOT"
  :description "a lively vi"
  :url "http://github.com/maitria/avi"
  :license {:name "avi license"
            :url "http://github.com/maitria/avi/README.md"}
  :jvm-opts ["-Djava.library.path=/usr/local/lib/avi"]
  :plugins [[lein-midje "3.0.0"]]
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :profiles {:dev {:dependencies [[midje "1.5.1"]]}
             :uberjar {:aot :all}}
  :aliases {"install" ^{:doc "Install avi on this machine"}
            ["do" "uberjar" ["run" "-m" "avi.install/install"]]}
  :java-source-paths ["java-src"]
  :main avi.core)

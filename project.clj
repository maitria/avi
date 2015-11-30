(defproject avi "0.1.7-SNAPSHOT"
  :description "a lively vi"
  :url "http://github.com/maitria/avi"
  :license {:name "avi license"
            :url "http://github.com/maitria/avi/README.md"}
  :jvm-opts ["-Djava.library.path=/usr/local/lib/avi"]
  :plugins [[lein-midje "3.0.0"]]
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [com.maitria/packthread "0.1.7"]
                 [prismatic/schema "1.0.2"]
                 [potemkin "0.4.1"]]
  :profiles {:dev {:dependencies [[midje "1.7.0"]
                                  [org.clojure/test.check "0.7.0"]
                                  [com.gfredericks/test.chuck "0.1.18"]]
                   :jvm-opts ["-XX:-OmitStackTraceInFastThrow"]}
             :uberjar {:aot :all}}
  :repl-options {:init-ns avi.repl}
  :aliases {"install" ^{:doc "Install avi on this machine"}
            ["do" "uberjar," "run" "-m" "avi.install/install"]}
  :java-source-paths ["java-src"]
  :main avi.core)

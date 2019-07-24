(defproject icfpc "2019.0.0"
  :dependencies
  [[org.clojure/clojure   "1.10.1" #_"1.9.0"]
   [org.clojure/data.json "0.2.6"]

   [primitive-math "0.1.6" :exclusions [org.clojure/clojure]]
   [criterium "0.4.5"]
   [clj-tuple "0.2.2" :exclusions [org.clojure/clojure]]
   [io.lacuna/bifurcan "0.1.0"]
   [org.clojure/data.int-map "0.2.4"]
   [vertigo "0.1.4" :exclusions [org.clojure/clojure]]
   ]
  :source-paths   ["src"]
  :resource-paths ["resources"]
  :main icfpc.main
  :jvm-opts  ^:replace ["-server"
                                        ;"-Xmx4g"
                        "-Xverify:none"
                                        ;"-XX:+PrintCompilation"
                                        ;"-XX:+UnlockDiagnosticVMOptions"
                                        ;"-XX:+PrintInlining"
                        ]
  :plugins [[io.taylorwood/lein-native-image "0.3.0"]]  

  :global-vars {*warn-on-reflection* true
                ; *unchecked-math* :warn-on-boxed
              }

  :native-image {:name "icfpc2019"
                 :opts ["--report-unsupported-elements-at-runtime"
                        "--initialize-at-build-time"
                        "--no-fallback"
                        "-H:ReflectionConfigurationFiles=reflectconfig.json"]
                 :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[com.clojure-goes-fast/clj-java-decompiler "0.2.1"]]}})

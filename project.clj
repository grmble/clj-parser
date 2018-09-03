(defproject clj-parser "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "https://github.com/grmble/clj-parser"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/tools.namespace "0.2.11"]
                 [funcool/cats "2.2.0"]
                 ]

  :aliases
  {;; trampoline does not work on windows
   ;; "fig" ["trampoline" "run" "-m" "figwheel.main"]
   "fig" ["run" "-m" "figwheel.main" "-b" "dev" "-r"]
   }

  :main ^:skip-aot clj-parser.core
  :target-path "target/%s"
  :jar-exclusions [#".*\.html"]

  :profiles {:dev
             {:main clj-parser.dev
              :source-paths ["dev"]

              :dependencies [[org.clojure/clojurescript "1.10.339"]
                             [com.bhauman/figwheel-main "0.1.8"]
                             [devcards "0.2.5"]
                             [cheshire "5.8.0"] ; for benchmark comparisons
                             ]

              ;; so cljs compilation results are picked up
              :resource-paths ["target"]
              :clean-targets ^{:protect false} ["target"]
              }
            }
  )

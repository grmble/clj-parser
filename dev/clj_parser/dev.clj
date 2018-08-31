(ns clj-parser.dev
  "Main namespace for dev build

  * Run it with `lein repl`
  * clj-parser.core is required as p
  * enjoy
  "
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.repl :refer :all]
   [clojure.tools.namespace.repl :refer [refresh]]
   [clj-parser.core :as p]
   ))

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
   [clojure.test :refer :all]
   [clojure.tools.namespace.repl :refer [refresh]]
   [cats.core :as m]
   [clj-parser.core :as p]
   [clj-parser.input :as i]
   [clj-parser.json :as json]
   ))

(defn T []
  (refresh)
  (run-tests
   'clj-parser.core-tests
   'clj-parser.input-tests
   'clj-parser.json-tests
   ))

(def xl-input (slurp "xl.json"))

(defn xl-parse []
  (time (p/run-parser (json/json) xl-input)))

(defn xxx [s]
  (p/run-parser
   (m/mlet [x (p/token (p/string "x"))
            _ (p/token (p/string ":"))
            y (p/token (p/string "y"))]
           (m/return (str x y)))
   s))

(defn yyy [s]
  (p/run-parser (json/key-and-value) s))

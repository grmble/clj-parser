(ns clj-parser.all-tests
  (:require
   [cljs.test :refer-macros [testing is run-tests]]
   [devcards.core :as dc :refer-macros [deftest]]
   [clj-parser.core-tests]
   [clj-parser.input-tests]
   [clj-parser.json-tests]
   ))

(dc/start-devcard-ui!)

(defn T []
  (run-tests
   'clj-parser.core-tests
   'clj-parser.input-tests
   'clj-parser.json-tests
   ))

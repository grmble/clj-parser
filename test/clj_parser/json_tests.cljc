(ns clj-parser.json-tests
  (:require
   #?(:clj [clojure.test :refer :all]
      :cljs [cljs.test :refer-macros [testing is]])
   #?(:cljs [devcards.core :refer-macros [deftest]])
   [cats.core :as m]
   [cats.builtin]
   [clj-parser.compat :as compat]
   [clj-parser.core :as p]
   [clj-parser.json :as json]
   )
  )

(deftest test-json
  (testing "initial whitespace"
    (is (nil? (p/parse json/json " null"))))
  (testing "final whitespace"
    (is (nil? (p/parse json/json "null   "))))
  (testing "number"
    (is (= 1 (p/parse json/json "1")))
    (is (= -1 (p/parse json/json "-1")))
    (is (= -1.5 (p/parse json/json "-1.5")))
    )
  (testing "string"
    (is (= "asdf" (p/parse json/json "\"asdf\"")))
    (is (= "nl:\n" (p/parse json/json "\"nl:\\n\"")))
    (is (= "cr:\r" (p/parse json/json "\"cr:\\r\"")))
    (is (= "bs:\b" (p/parse json/json "\"bs:\\b\"")))
    (is (= "bs:\\" (p/parse json/json "\"bs:\\\\\"")))
    (is (= "tb:\t" (p/parse json/json "\"tb:\\t\"")))
    (is (= "sp: " (p/parse json/json "\"sp:\\u0020\"")))
    )
  (testing "array"
    (is (= [] (p/parse json/json "[]")))
    (is (= [nil] (p/parse json/json "[ null ]")))
    (is (= [1] (p/parse json/json "[1]")))
    (is (= [1 2] (p/parse json/json "[1, 2]")))
    )
  (testing "object"
    (is (= {} (p/parse json/json "{}")))
    (is (= {"a" 1} (p/parse json/json "{\"a\": 1}")))
    (is (= {"a" 1 "b" 2} (p/parse json/json "{\"a\": 1, \"b\": 2}")))
    )
  )

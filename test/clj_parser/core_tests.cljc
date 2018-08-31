(ns clj-parser.core-tests
  (:require
   #?(:clj [clojure.test :refer :all]
      :cljs [cljs.test :refer-macros [testing is]])
   #?(:cljs [devcards.core :refer-macros [deftest]])
   [clj-parser.core :as p]
   ))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 0))))

(ns clj-parser.core-tests
  (:require
   #?(:clj [clojure.test :refer :all]
      :cljs [cljs.test :refer-macros [testing is]])
   #?(:cljs [devcards.core :refer-macros [deftest]])
   #?(:cljs [cljs.reader :refer [read-string]]
      :clj [clojure.edn :refer [read-string]])
   [cats.core :as m]
   [cats.builtin]
   [clj-parser.input :as input]
   [clj-parser.core :refer [parse success string regexp eof many optional run-parser success?
                            many1 token sep-by]])
  (:refer-clojure :exclude [read-string])
  )

(deftest test-primitives
  (testing "primitive: success"
    (is (= {:tag "success"} (parse (success {:tag "success"}) "")))
    (is (= {:tag "success"} (parse (success {:tag "success"}) "has more input"))))
  (testing "primitive: string"
    (is (= "foo" (parse (string "foo") "foo")))
    (is (= "foo" (parse (string "foo") "foo has more input")))
    (is (= nil (parse (string "foo") "")))
    (is (= nil (parse (string "foo") "xfoo")))
    (is (= nil (parse (string "foo") "fox"))))
  (testing "primitive: regexp"
    (is (= nil (parse (regexp #"abc") "")))
    (is (= "" (parse (regexp #"a*") "")))
    (is (= nil (parse (regexp #"abc") "xabc")))
    (is (= nil (parse (regexp #"abc") "ab")))
    (is (= nil (parse (regexp #"abc") "abx")))
    (is (= "abc" (parse (regexp #"abc") "abc")))
    (is (= "abc" (parse (regexp #"abc") "abc has more input"))))
  (testing "primitive: eof"
    (is (= "" (parse (eof) "")))
    (is (= nil (parse (eof) "has more input"))))
  (testing "primitive: many"
    (is (= ["a" "a" "a"] (parse (many (string "a")) "aaa")))
    (is (= ["a" "a" "a"] (parse (many (string "a")) "aaa has more input")))
    (is (= ["a" "a"] (parse (many (string "a")) "aa")))
    (is (= ["a"] (parse (many (string "a")) "a")))
    (is (= [] (parse (many (string "a")) ""))))
  (testing "primitive: optional"
    (is (= "a" (parse (optional (string "a")) "a")))
    (is (= "a" (parse (optional (string "a")) "a has more input")))
    (let [result (run-parser (optional (string "a")) (input/->StringInput ""))]
      (is (success? result))
      (is (= nil (get result :obj)))))
  )

(deftest test-combinators
  (testing "combinator: many1"
    (is (= ["a" "a" "a"] (parse (many1 (string "a")) "aaa")))
    (is (= ["a" "a" "a"] (parse (many1 (string "a")) "aaa has more input")))
    (is (= ["a" "a"] (parse (many1 (string "a")) "aa")))
    (is (= ["a"] (parse (many1 (string "a")) "a")))
    (is (= nil (parse (many1 (string "a")) ""))))
  (testing "combinator: token"
    (is (= "a" (parse (token (string "a")) "a")))
    (is (= "a" (parse (token (string "a")) "a   "))))
  (testing "combinator: sep-by"
    (is (= ["a", "a", "a"] (parse (sep-by (string "a") ",") "a, a, a")))
    (is (= ["a", "a", "a"] (parse (sep-by (string "a") ",") "a,a,a")))
    (is (= ["a", "a", "a"] (parse (sep-by (string "a") ",") "a  ,  a   ,  a   ")))
    (is (= ["a", "a"] (parse (sep-by (string "a") ",") "a, a")))
    (is (= ["a"] (parse (sep-by (string "a") ",") "a")))
    (is (= [] (parse (sep-by (string "a") ",") "")))
    )
  )

(deftest test-cats
  (testing "fmap"
    (is (= 123 (parse (m/fmap #(read-string %) (string "123")) "123 and more"))))
  (testing "ap"
    (is (= "asdfjkl" (parse (m/ap str (string "asdf") (string "jkl")) "asdfjkl"))))
  (testing "bind"
    (is (= "asdfjkl" (parse (m/mlet [a (string "asdf")
                                     j (string "jkl")]
                                    (m/return (str a j)))
                            "asdfjkl"))))
  (testing "mplus"
    (is (= "asdf" (parse (m/mplus (string "asdf") (string "jkl")) "asdf")))
    (is (= "jkl" (parse (m/mplus (string "asdf") (string "jkl")) "jkl")))
    (is (= nil (parse (m/mplus (string "asdf") (string "jkl")) "")))
    (is (= nil (parse (m/mplus (string "asdf") (string "jkl")) "none of the above")))
    ;; the successful first parser commits the result, mplus fails
    (is (= nil (parse (m/mplus (m/ap str (string "as") (string "xx")) (string "asdf")) "asdfjkl")))
    ))

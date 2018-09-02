(ns clj-parser.input-tests
  (:require
   #?(:clj [clojure.test :refer :all]
      :cljs [cljs.test :refer-macros [testing is]])
   #?(:cljs [devcards.core :refer-macros [deftest]])
   [clj-parser.input :as i])
  )

(deftest string-prefix-test
  "Matching strings against a string input"
  (let [inp (i/->StringInput "abc")]
    (testing "match on first character"
      (is (= "a" (i/string-prefix-at? inp 0 "a")))
      (is (= "ab" (i/string-prefix-at? inp 0 "ab")))
      (is (= "abc" (i/string-prefix-at? inp 0 "abc")))
      (is (nil? (i/string-prefix-at? inp 0 "x")))
      (is (nil? (i/string-prefix-at? inp 0 "abcX"))))

    (testing "match on second character"
      (is (nil? (i/string-prefix-at? inp 0 "b")))
      (is (= "b" (i/string-prefix-at? inp 1 "b")))
      (is (= "bc" (i/string-prefix-at? inp 1 "bc")))
      (is (nil? (i/string-prefix-at? inp 1 "x")))
      (is (nil? (i/string-prefix-at? inp 1 "bcX"))))

    (testing "match on last character"
      (is (nil? (i/string-prefix-at? inp 1 "c")))
      (is (= "c" (i/string-prefix-at? inp 2 "c")))
      (is (nil? (i/string-prefix-at? inp 2 "cX")))
      (is (nil? (i/string-prefix-at? inp 2 "x"))))

    (testing "match past last character"
      (is (nil? (i/string-prefix-at? inp 3 "x"))))

    (testing "matching an empty string"
      (is (= "" (i/string-prefix-at? inp 2 "")))
      (is (= "" (i/string-prefix-at? inp 3 ""))))))

(deftest character-prefix-test
  "Matching single characters against a string input"
  (let [inp (i/->StringInput "abc")]
    (testing "matching the first character"
      (is (= \a (i/char-prefix-at? inp 0 \a)))
      (is (nil? (i/char-prefix-at? inp 0 \b))))
    (testing "matching the first character"
      (is (= \b (i/char-prefix-at? inp 1 \b)))
      (is (nil? (i/char-prefix-at? inp 1 \c))))
    (testing "matching the first character"
      (is (= \c (i/char-prefix-at? inp 2 \c)))
      (is (nil? (i/char-prefix-at? inp 2 \d))))
    (testing "matching past last character"
      (is (nil? (i/char-prefix-at? inp 3 \x)))
      )))

(deftest regexp-prefix-test
  "Matching regular expressions against a string input"
  (let [inp (i/->StringInput "abc")]
    (testing "succesful matches"
      (is (= ["a"] (i/re-prefix-at? inp 0 #"a")))
      (is (= ["b"] (i/re-prefix-at? inp 1 #"b")))
      (is (= ["c"] (i/re-prefix-at? inp 2 #"c"))))
    (testing "does not match"
      (is (nil? (i/re-prefix-at? inp 0 #"b")))
      (is (nil? (i/re-prefix-at? inp 1 #"a"))))
    (testing "matching past last character"
      (is (nil? (i/re-prefix-at? inp 3 #"x"))))
    (testing "matching empty regex"
      (is (= [""] (i/re-prefix-at? inp 0 #"")))
      (is (= [""] (i/re-prefix-at? inp 3 #"")))
      )
    ))

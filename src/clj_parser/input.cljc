(ns clj-parser.input
  (:require
   [clj-parser.compat :as compat])
  )

#?(:clj (set! *warn-on-reflection* true))

(defprotocol Input
  "Parsing input"
  (exhausted? [this pos]
    "Is the input exhausted at the given position?")
  (char-prefix-at? [this pos ^Character prefix]
    "Does the character match the input at the given position?")
  (string-prefix-at? [this pos ^String prefix]
    "Does the string match the input at the given position?"))

(defprotocol RegexInput
  "Parsing input that supports regular expressions.

  This means
  * synchronous
  * string"
  (re-prefix-at? [this pos prefix]
    "Is the regular expression a prefix starting at pos?"))

(defrecord StringInput [^String input]
  Input
  (exhausted? [_ pos]
    (<= (count input) pos))
  (char-prefix-at? [_ pos char]
    (when (and
           (< pos (count input))
           (= char (nth input pos)))
      char))
  (string-prefix-at? [_ pos prefix]
    (when (.startsWith input prefix pos)
      prefix))
  RegexInput
  (re-prefix-at? [_ pos prefix]
    (compat/str-regex-at? input pos prefix)))

(ns clj-parser.compat
  (:require
   [cljs.reader :as r]
  ))

(defn str-regex-at? [str pos prefix]
  (let [prefix2 (js/RegExp. prefix "y")] ; make it sticky
    ;; if prefix2 is called prefix as well, this fails
    ;; huh?
    (set! prefix2.lastIndex pos)
    (seq (.exec prefix2 str))))

(def read-string r/read-string)

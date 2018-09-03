(ns clj-parser.compat
  (:require
   [clojure.edn :as edn]
   )
  (:refer-clojure :exclude [read-string])
  )

(set! *warn-on-reflection* true)

(defn str-regex-at? [^String str pos ^java.util.regex.Pattern prefix]
  (let [matcher (.matcher prefix str)]
    (.region matcher pos (count str))
    (when (.lookingAt matcher)
      (.group matcher))))

(def read-string edn/read-string)

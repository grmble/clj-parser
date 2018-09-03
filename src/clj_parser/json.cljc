(ns clj-parser.json
  (:require [clj-parser.core :as p]
            [clj-parser.compat :as c]
            [cats.core :as m])
  (:refer-clojure :exclude [array])
  )

#?(:clj (set! *warn-on-reflection* true))

(def null
  (m/fmap (constantly nil) (p/token (p/string "null"))))

(def jtrue
  (m/fmap (constantly true) (p/token (p/string "true"))))

(def jfalse
  (m/fmap (constantly false) (p/token (p/string "false"))))

(def ^{:doc
       "Parse a JSON number

  https://stackoverflow.com/questions/13340717/json-numbers-regular-expression
  "}
  number
  (m/fmap #(c/read-string %) (p/token (p/regexp #"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?"))))


#?(:clj
   (defn- handle-string-body [^String s]
     (let [sb (StringBuilder.)
           end (dec (count s))]
       (loop [idx 0]
         (if (< idx end)
           (let [c1 (.charAt s (int idx))]
             (if (= \\ c1)
               (let [c2 (.charAt s (int (inc idx)))]
                 (if (= \u c2)
                   (let [digits (.substring s (+ idx 2) (+ idx 6))]
                     (.append sb (char (Integer/parseInt digits 16)))
                     (recur (+ idx 6)))
                   (do
                     (case c2
                       \\ (.append sb \\)
                       \" (.append sb \")
                       \/ (.append sb \/)
                       \b (.append sb "\b")
                       \f (.append sb "\f")
                       \n (.append sb "\n")
                       \r (.append sb "\r")
                       \t (.append sb "\t"))
                     (recur (+ idx 2)))))
               (do
                 (.append sb c1)
                 (recur (inc idx)))))
           (.toString sb)))))

   :cljs
   (defn- handle-string-body [s]
     (let [sb #js[]
           end (dec (count s))]
       (loop [idx 0]
         (if (< idx end)
           (let [c1 (get s idx)]
             (if (= \\ c1)
               (let [c2 (get s (inc idx))]
                 (if (= \u c2)
                   (let [digits (subs s (+ idx 2) (+ idx 6))]
                     (.push sb (char (c/read-string (str "0x" digits))))
                     (recur (+ idx 6)))
                   (do
                     (case c2
                       \\ (.push sb \\)
                       \" (.push sb \")
                       \/ (.push sb \/)
                       \b (.push sb "\b")
                       \f (.push sb "\f")
                       \n (.push sb "\n")
                       \r (.push sb "\r")
                       \t (.push sb "\t"))
                     (recur (+ idx 2)))))
               (do
                 (.push sb c1)
                 (recur (inc idx)))))
           (.join sb "")))))
   )

(declare inner-json)

(def ^:private double-quote (p/character \"))
(def ^:private open-curly (p/token (p/character \{)))
(def ^:private close-curly (p/token (p/character \})))
(def ^:private open-bracket (p/token (p/character \[)))
(def ^:private close-bracket (p/token (p/character \])))
(def ^:private colon (p/token (p/character \:)))
(def ^:private comma (p/token (p/character \,)))
(def ^:private inner-string
  (m/fmap handle-string-body
          (p/token (p/regexp #"(?:[^\\\"]*|\\[\\\"/bfnrt]|\\u[0-9a-fA-F]{4})*\""))))

(def string
  (m/mlet [_ double-quote
           s inner-string]
          (m/return s)))


(def array
  (m/mlet [_ open-bracket
           arr (p/sep-by inner-json comma)
           _ close-bracket]
          (m/return arr)))

(def ^:private
  key-and-value
  (m/mlet [k string
           _ colon
           v inner-json]
    (m/return [k v])))

(def object
  (m/mlet [_ open-curly
           kvs (p/sep-by key-and-value comma)
           _ close-curly]
          (m/return (into {} kvs))))

(def ^:private
  inner-json
  (m/mplus
   string
   number
   array
   object
   null
   jtrue
   jfalse
   (p/fail "Must be one of: {, [, \", a number, true, false or null")
   ))

(def json
  (m/mlet [_ p/whitespace
           x inner-json
           _ p/eof]
          (m/return x)))

(ns clj-parser.json
  (:require [clj-parser.core :as p]
            [clj-parser.compat :as c]
            [cats.core :as m])
  (:refer-clojure :exclude [array])
  )

(defn null
  []
  (m/fmap (constantly nil) (p/token (p/string "null"))))

(defn number
  "Parse a JSON number

  https://stackoverflow.com/questions/13340717/json-numbers-regular-expression
  "
  []
  (m/fmap #(c/read-string %) (p/token (p/regexp #"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?"))))


#?(:clj
   (defn- handle-string-body [s]
     (let [sb (StringBuilder.)
           end (dec (.length s))]
       (loop [idx 1]
         (if (< idx end)
           (let [c1 (.charAt s idx)]
             (if (= \\ c1)
               (let [c2 (.charAt s (inc idx))]
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
       (loop [idx 1]
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


(defn string
  "Parse a JSON String"
  []
  (m/fmap handle-string-body
          (p/token (p/regexp #"\"(?:[^\\\"]*|\\[\\\"/bfnrt]|\\u[0-9a-fA-F]{4})*\""))))

(declare inner-json)

(defn array
  "Parse a JSON Array"
  []
  (m/mlet [_ (p/token (p/string "["))
           arr (p/sep-by (inner-json) ",")
           _ (p/token (p/string "]"))]
          (m/return arr)))

(defn- key-and-value []
  (m/mlet [k (p/token (string))
           _ (p/token (p/string ":"))
           v (inner-json)]
    (m/return [k v])))

(defn object
  "Parse a JSON object"
  []
  (m/mlet [_ (p/token (p/string "{"))
           kvs (p/sep-by (key-and-value) ",")
           _ (p/token (p/string "}"))]
          (m/return (into {} kvs))))


(defn- inner-json
  []
  (m/mplus
   (string)
   (number)
   (array)
   (object)
   (null)
   ))

(defn json
  "Parse JSON"
  []
  (m/mlet [_ (p/regexp #"\s*")
           x (inner-json)
           _ (p/eof)]
          (m/return x)))

(ns clj-parser.core
  "The Parser Monad

  It's a function given a input string and current position, returning a result
  - either a success or a failure.
  "
  (:require
   [clj-parser.input :as input]
   [cats.core :as m]
   [cats.protocols :as p]
   [cats.util :as util]))

(declare context)

(defrecord Parser [parser-fn]

  p/Contextual
  (-get-context [_] context)

  p/Extract
  (-extract [_] parser-fn)

  p/Printable
  (-repr [_] (str "#<Parser " (pr-str parser-fn) ">"))

  #?@(:cljs [cljs.core/IDeref
             (-deref [_] parser-fn)]
      :clj [clojure.lang.IDeref
            (deref [_] parser-fn)])
  )

(alter-meta! #'->Parser assoc :private true)
(util/make-printable Parser)

;;;
;;;
;;; utils
;;;
;;;
;;; XXX: delete this when improving failure
(defn- safe-subs
  [s beg end]
  (let [n (count s)
        beg (min (dec n) beg)
        end (min (dec n) end)
        ]
    (if (zero? n)
      ""
      (.substring s beg end))))


;;;
;;;
;;;  Parse Results
;;;
;;;

(defprotocol Result
  "The result of a parse, either Success or Failure"

  (success? [res] "Is it a success?")
  (committed? [res] "Is the result committed?")
  (^long position [res] "Position of the result")
  (advance [res ^long delta] "Advance the position of a successful parse")
  )

(defrecord Success [obj ^long pos]
  Result
  (success? [res] res)
  (committed? [_] true)
  (position [_] pos)
  (advance [res delta] (assoc res :pos (+ pos delta))))

(defrecord Failure [^String msg ^String exp ^String act ^long pos ^boolean com]
  Result
  (success? [_] nil)
  (committed? [_] com)
  (position [_] pos)
  (advance [res delta] res))

(defn ^:private handle-result
  "Handle a result - for success, call the success function, else the failure function"
  [res succ-fn fail-fn]
  (if (success? res)
    (succ-fn res)
    (fail-fn res)))

(defn ^:private handle-success
  "Handle a success by calling the success function.  Returns the failure on failure."
  [res succ-fn]
  (if (success? res)
    (succ-fn res)
    res))

(alter-meta! #'->Success assoc :private true)
(alter-meta! #'->Failure assoc :private true)

;;;
;;;
;;; Parser Primitives
;;;
;;;

(defn
  ^:private
  run-parser-pos [parser input ^long pos committed]
  ((p/-extract parser) input pos committed))


(defn run-parser
  "Run a parser and obtain a success of failure"
  [parser input]
  (run-parser-pos parser input 0 false))


(defn parse
  "Run the parser and return the content of the success, or nil on error"
  [parser ^String input]
  (handle-result
    (run-parser parser (input/->StringInput input))
    #(get % :obj)
    (fn [_] nil)))

(defn string
  "Parses the string argument

  If the input contains the string at the current position,
  the parse succeeds."
  [^String s]
  (->Parser
    (fn [input ^long pos com]
      (if (input/string-prefix-at? input pos s)
        (->Success s (+ pos (count s)))
        (->Failure "String did not match" s
                       (safe-subs (:input input) pos (+ pos (count s)))
                       pos
                       com)))))

(defn regexp
  "Parse the input using a regexp.

  If the input matches the regexp at the current position,
  the parse succeeds."
  [re]
  (->Parser
    (fn [input ^long pos com]
      (if-let [groups (input/re-prefix-at? input pos re)]
        (->Success (first groups) (+ pos (count (first groups))))
        (->Failure "Regexp did not match" (str re)
                   (safe-subs (:input input) pos (+ pos 10))
                   pos
                   com)))))

(defn eof
  "Matches the end of input"
  []
  (->Parser
    (fn [input ^long pos com]
      (if (input/exhausted? input pos)
        (->Success "" pos)
        (->Failure "Input not exhausted"
                   ""
                   (safe-subs (:input input) pos (+ pos 10))
                   pos
                   com)))))


(defn success
  "A succesful parse containing the given object."
  [obj]
  (->Parser
    (fn [_ ^long pos com]
      (->Success obj pos))))


(defn many
  "Parse as many repetitions of parser as possible.

  This will never fail - instead gives empty vector.
  See many1 which will fail or return vector with at least 1
  parsed element."
  [parser]
  (->Parser
    (fn [input ^long pos com]
      (loop [pos pos
             com com
             acc []]
        (let [result (run-parser-pos parser input pos com)]
          (if (success? result)
            (recur (long (position result)) true (conj acc (get result :obj)))
            (->Success acc pos)))))))

(defn optional
  "Optional parser.  Returns result or nil success"
  [parser]
  (->Parser
    (fn [input ^long pos com]
      (handle-result
        (run-parser-pos parser input pos com)
        identity
        (fn [_] (->Success nil pos))))))

;;;
;;;
;;; parser combinators
;;;
;;;
(defn many1
  [parser]
  (m/mlet [x1 parser
           xs (many parser)]
          (m/return (into [x1] xs))))

(defn token [parser]
  (m/mlet [tok parser
           ws  (regexp #"\s*")]
          (m/return tok)))

(defn ^:private sep-then [str parser]
  (m/mlet [_ (token (string str))
           tok (token parser)]
          (m/return tok)))

(defn sep-by1 [parser str]
  (m/mlet [x1 (token parser)
           xs (many (sep-then str parser))]
          (m/return (into [x1] xs))))

(defn sep-by [parser str]
  (m/mplus (sep-by1 parser str)
           (success [])))




(def ^{:no-doc true}
  context
  (reify
    p/Context

    p/Functor
    (-fmap [_ func parser]
      (->Parser
        (fn [input ^long pos com]
          (handle-success
            (run-parser-pos parser input pos com)
            (fn [res] (update res :obj func))))))

    p/Applicative
    (-pure [_ obj]
      (success obj))

    (-fapply [m af av]
      (->Parser
        (fn [input ^long pos com]
          (handle-success
            (run-parser-pos af input pos com)
            (fn [res]
              (let [func (get res :obj)
                    pos2 (get res :pos)]
                (handle-success
                  (run-parser-pos av input pos2 true)
                  (fn [res2]
                    (update res2 :obj func)))))))))


    p/Monad
    (-mreturn [_ obj]
      (success obj))

    (-mbind [_ parser func]
      (->Parser
        (fn [input ^long pos com]
          (handle-success
            (run-parser-pos parser input pos com)
            (fn [res]
              (let [pos2 (get res :pos)
                    parser2 (func (get res :obj))]
                (run-parser-pos parser2 input pos2 true)))))))

    p/MonadPlus
    (-mplus [_ parser1 parser2]
      (->Parser
        (fn [input ^long pos com]
          (handle-result
            (run-parser-pos parser1 input pos com)
            (fn [res] res)
            (fn [res]
              (if (committed? res)
                res
                (run-parser-pos parser2 input pos false)))))))
    ))


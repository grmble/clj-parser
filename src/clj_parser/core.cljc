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
;;; Parser State
;;;
;;;

(defprotocol IState
  "A state can be advanced"
  (committed? [this]
    "Is the parse commited?")
  (get-position [this]
    "Get the current position")
  (advance [this delta]
    "Advance the parsing state by delta character positions. Commits the parse"))

(declare ->State)

(deftype State [^long pos committed]
  IState
  (committed? [_] committed)
  (get-position [_] pos)
  (advance [_ delta]
    (->State (+ pos delta) true))
  Object
  (toString [_] (str "<State :pos " pos " :committed " committed ">"))
  )

;;;
;;;
;;; Parse Results
;;;
;;;

(defprotocol Result
  "The result of a parse, either Success or Failure"

  (success? [res] "Is it a success?")
  (get-state [res] "Get the parser state")
  (get-result [res] "The result on success, nil otherwise")
  (get-error [res] "The error stack on failure, nil otherwise")
  )

(deftype Success [state obj]
  Result
  (success? [res] res)
  (get-state [_] state)
  (get-result [_] obj)
  (get-error [_] nil)
  IState
  (committed? [_] (committed? state))
  (get-position [_] (get-position state))
  (advance [_ x] (advance state x))
  Object
  (toString [_] (str "<Success :state " state " :result " obj ">"))
  )

(defrecord StackEntry [^String msg ^long pos])

(deftype Failure [state stack]
  Result
  (success? [_] nil)
  (get-state [_] state)
  (get-result [_] nil)
  (get-error [_] stack)
  IState
  (committed? [_] (committed? state))
  (get-position [_] (get-position state))
  (advance [_ x] (advance state x))
  Object
  (toString [_] (str "<Failure :state " state " :stack " stack ">"))
  )

(defn- handle-result
  "Handle a result - for success, call the success function, else the failure function"
  [res succ-fn fail-fn]
  (if (success? res)
    (succ-fn res)
    (fail-fn res)))

(defn- handle-success
  "Handle a success by calling the success function.  Returns the failure on failure."
  [res succ-fn]
  (if (success? res)
    (succ-fn res)
    res))

(alter-meta! #'->Success assoc :private true)
(alter-meta! #'->Failure assoc :private true)

(defn- failure [state msg]
  (->Failure state (list (->StackEntry msg (get-position state)))))



;;;
;;;
;;; Parser Primitives
;;;
;;;

(defn
  ^:private
  run-parser-with-state [parser input state]
  ((p/-extract parser) input state))


(defn run-parser
  "Run a parser and obtain a success of failure"
  [parser input]
  (let [input (input/make-input input)]
    (run-parser-with-state parser input (->State 0 false))))


(defn parse
  "Run the parser and return the content of the success, or nil on error"
  [parser ^String input]
  (handle-result
    (run-parser parser input)
    get-result
    (constantly nil)))

(defn character
  "Parses the character.

  Returns the character on success, nil otherwise."
  [^Character ch]
  (->Parser
   (fn [input state]
     (if (input/char-prefix-at? input (get-position state) ch)
       (->Success (advance state 1) ch)
       (failure state (str "Character did not match: " ch))))))


(defn string
  "Parses the string argument

  If the input contains the string at the current position,
  the parse succeeds."
  [^String s]
  (->Parser
    (fn [input state]
      (if (input/string-prefix-at? input (get-position state) s)
        (->Success (advance state (count s)) s)
        (failure state (str "String did not match: " s))))))

(defn regexp
  "Parse the input using a regexp.

  If the input matches the regexp at the current position,
  the parse succeeds."
  [re]
  (->Parser
    (fn [input state]
      (if-let [result (input/re-prefix-at? input (get-position state) re)]
        (->Success (advance state (count result)) result)
        (failure state (str "Regexp did not match: " (str re)))))))

(def ^{:doc "Matches the end of input"}
  eof
  (->Parser
    (fn [input state]
      (if (input/exhausted? input (get-position state))
        (->Success state :eof)
        (failure state "Input not exhausted")))))


(defn success
  "A succesful parse containing the given object."
  [obj]
  (->Parser
    (fn [_ state]
      (->Success state obj))))

(defn fail
  "A failed parse"
  [msg]
  (->Parser
   (fn [_ state]
     (failure state msg))))

(defn many
  "Parse as many repetitions of parser as possible.

  This will never fail - instead gives empty vector.
  See many1 which will fail or return vector with at least 1
  parsed element."
  [parser]
  (->Parser
    (fn [input state]
      (loop [state state
             acc []]
        (let [result (run-parser-with-state parser input state)]
          (if (success? result)
            (recur (get-state result) (conj acc (get-result result)))
            (->Success state acc)))))))

(defn optional
  "Optional parser.  Returns result or empty-result (default: nil) on success"
  ([parser empty-result]
   (->Parser
    (fn [input state]
      (handle-result
       (run-parser-with-state parser input state)
       identity
       (fn [res]
         (if (committed? res)
           res
           (->Success state nil)))))))
  ([parser]
   (optional parser nil)))

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

(def ^{:doc "A parser for whitespace"}
  whitespace
  (regexp #"\s*"))


(defn token [parser]
  (m/mlet [tok parser
           ws  whitespace]
          (m/return tok)))

(defn ^:private sep-then [sep parser]
  (m/mlet [_ sep
           x parser]
          (m/return x)))

(defn sep-by1 [parser sep]
  (m/mlet [x1 parser
           xs (many (sep-then sep parser))]
          (m/return (into [x1] xs))))

(defn sep-by [parser sep]
  (m/mplus (sep-by1 parser sep)
           (success [])))



(def ^{:no-doc true}
  context
  (reify
    p/Context

    p/Functor
    (-fmap [_ func parser]
      (->Parser
        (fn [input state]
          (handle-success
            (run-parser-with-state parser input state)
            (fn [res] (->Success (get-state res) (func (get-result res))))))))

    p/Applicative
    (-pure [_ obj]
      (success obj))

    (-fapply [m af av]
      (->Parser
        (fn [input state]
          (handle-success
            (run-parser-with-state af input state)
            (fn [res]
              (let [func (get-result res)
                    state2 (get-state res)]
                (handle-success
                  (run-parser-with-state av input state2)
                  (fn [res2]
                    (->Success (get-state res2) (func (get-result res2)))))))))))


    p/Monad
    (-mreturn [_ obj]
      (success obj))

    (-mbind [_ parser func]
      (->Parser
        (fn [input state]
          (handle-success
            (run-parser-with-state parser input state)
            (fn [res]
              (let [state2 (get-state res)
                    parser2 (func (get-result res))]
                (run-parser-with-state parser2 input state2)))))))

    p/MonadPlus
    (-mplus [_ parser1 parser2]
      (->Parser
        (fn [input state]
          (let [state (->State (get-position state) false)]
            (handle-result
             (run-parser-with-state parser1 input state)
             identity
             (fn [res]
               (if (committed? res)
                 res
                 (run-parser-with-state parser2 input state))))))))
    ))


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
  (advance [this delta]
    "Advance the parsing state by delta character positions"))

(declare ->State)

(defrecord State [^long pos committed]
  IState
  (advance [_ delta]
    (->State (+ pos delta) true)))

;;;
;;;
;;; Parse Results
;;;
;;;

(defprotocol Result
  "The result of a parse, either Success or Failure"

  (success? [res] "Is it a success?")
  )

(defrecord Success [state obj]
  Result
  (success? [res] res))

(defrecord StackEntry [^String msg ^long pos])

(defrecord Failure [state stack]
  Result
  (success? [_] nil))

(defn handle-result
  "Handle a result - for success, call the success function, else the failure function"
  [res succ-fn fail-fn]
  (if (success? res)
    (succ-fn res)
    (fail-fn res)))

(defn handle-success
  "Handle a success by calling the success function.  Returns the failure on failure."
  [res succ-fn]
  (if (success? res)
    (succ-fn res)
    res))

(alter-meta! #'->Success assoc :private true)
(alter-meta! #'->Failure assoc :private true)

(defn failure [state msg]
  (->Failure state (list (->StackEntry msg (:pos state)))))



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
    #(get % :obj)
    (fn [_] nil)))

(defn string
  "Parses the string argument

  If the input contains the string at the current position,
  the parse succeeds."
  [^String s]
  (->Parser
    (fn [input state]
      (if (input/string-prefix-at? input (:pos state) s)
        (->Success (advance state (count s)) s)
        (failure state (str "String did not match: " s))))))

(defn regexp
  "Parse the input using a regexp.

  If the input matches the regexp at the current position,
  the parse succeeds."
  [re]
  (->Parser
    (fn [input state]
      (if-let [groups (input/re-prefix-at? input (:pos state) re)]
        (->Success (advance state (count (first groups))) (first groups))
        (failure state (str "Regexp did not match: " (str re)))))))

(defn eof
  "Matches the end of input"
  []
  (->Parser
    (fn [input state]
      (if (input/exhausted? input (:pos state))
        (->Success state :eof)
        (failure state "Input not exhausted")))))


(defn success
  "A succesful parse containing the given object."
  [obj]
  (->Parser
    (fn [_ state]
      (->Success state obj))))

(defn fail
  "A failed, committed parse"
  [msg]
  (->Parser
   (fn [_ state]
     (failure (assoc state :committed true) msg))))

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
            (recur (:state result) (conj acc (:obj result)))
            (->Success state acc)))))))

(defn optional
  "Optional parser.  Returns result or nil success"
  [parser]
  (->Parser
    (fn [input state]
      (handle-result
        (run-parser-with-state parser input state)
        identity
        (fn [_] (->Success state nil))))))

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
        (fn [input state]
          (handle-success
            (run-parser-with-state parser input state)
            (fn [res] (update res :obj func))))))

    p/Applicative
    (-pure [_ obj]
      (success obj))

    (-fapply [m af av]
      (->Parser
        (fn [input state]
          (handle-success
            (run-parser-with-state af input state)
            (fn [res]
              (let [func (:obj res)
                    state2 (:state res)]
                (handle-success
                  (run-parser-with-state av input state2)
                  (fn [res2]
                    (update res2 :obj func)))))))))


    p/Monad
    (-mreturn [_ obj]
      (success obj))

    (-mbind [_ parser func]
      (->Parser
        (fn [input state]
          (handle-success
            (run-parser-with-state parser input state)
            (fn [res]
              (let [state2 (:state res)
                    parser2 (func (:obj res))]
                (run-parser-with-state parser2 input state2)))))))

    p/MonadPlus
    (-mplus [_ parser1 parser2]
      (->Parser
        (fn [input state]
          (let [state (assoc state :committed false)]
            (handle-result
             (run-parser-with-state parser1 input state)
             identity
             (fn [res]
               (if (:committed (:state res))
                 res
                 (run-parser-with-state parser2 input state))))))))
    ))


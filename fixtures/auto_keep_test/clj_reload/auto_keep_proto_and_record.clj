(ns clj-reload.auto-keep-proto-and-record)

;; Both protocol and record in the same ns, for testing combined auto-keep
(defprotocol ICombined
  :extend-via-metadata true
  (-combined-method [_]))

(defrecord CombinedRec [x]
  ICombined
  (-combined-method [_] :inline-impl))

(def combined-rec (CombinedRec. 42))

(defrecord ExtRec [])
(def ext-rec (ExtRec.))
(extend-protocol ICombined
  ExtRec
  (-combined-method [_] :ext-impl))

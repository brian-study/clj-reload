(ns clj-reload.auto-keep-protocol)

;; No ^:clj-reload/keep annotation — relies on :auto-keep config
(defprotocol IAutoProto
  :extend-via-metadata true
  (-auto-method [_]))

;; ---

(defrecord RecInline []
  IAutoProto
  (-auto-method [_]
    :rec-inline))

(def rec-inline
  (RecInline.))

;; ---

(defrecord RecExtendProto [])

(def rec-extend-proto
  (RecExtendProto.))

(extend-protocol IAutoProto
  RecExtendProto
  (-auto-method [_]
    :rec-extend-proto))

;; ---

(defrecord RecExtendType [])

(def rec-extend-type
  (RecExtendType.))

(extend-type RecExtendType
  IAutoProto
  (-auto-method [_]
    :rec-extend-type))

;; ---

(def extend-meta
  ^{'clj-reload.auto-keep-protocol/-auto-method
    (fn [_]
      :extend-meta)} [])

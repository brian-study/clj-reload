(ns clj-reload.auto-keep-downstream
  (:require [clj-reload.auto-keep-upstream]))

;; Protocol defined in a namespace that depends on upstream
(defprotocol IDownstreamProto
  (-downstream-method [_]))

(defrecord DownstreamRec []
  IDownstreamProto
  (-downstream-method [_] :downstream))

(def downstream-rec (DownstreamRec.))
(def downstream-val (rand-int Integer/MAX_VALUE))

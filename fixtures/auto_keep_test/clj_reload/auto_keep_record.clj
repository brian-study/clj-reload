(ns clj-reload.auto-keep-record)

;; No ^:clj-reload/keep — relies on :auto-keep config
(defrecord AutoRecord [t])

(def record-new
  (AutoRecord. 0))

(def record-factory
  (->AutoRecord 0))

(def record-map-factory
  (map->AutoRecord {:t 0}))

(ns clj-reload.auto-keep-type)

;; No ^:clj-reload/keep — relies on :auto-keep config
(deftype AutoType [t]
  java.lang.Object
  (equals [_ o]
    (and (instance? AutoType o)
      (= (.-t ^AutoType o) t))))

(def type-new
  (AutoType. 0))

(def type-factory
  (->AutoType 0))

(ns clj-reload.auto-keep-test
  (:require
    [clojure.test :refer [is deftest testing use-fixtures]]
    [clj-reload.core :as reload]
    [clj-reload.test-util :as tu]))

(defn wrap-test [f]
  (binding [tu/*dir* "fixtures/auto_keep_test"]
    (tu/reset
      '[clj-reload.auto-keep-protocol
        clj-reload.auto-keep-record
        clj-reload.auto-keep-type])
    (f)))

(use-fixtures :each wrap-test)

;; ============================================================
;; Protocol auto-keep
;; ============================================================

(deftest auto-keep-protocol-touch-test
  (testing "With :auto-keep #{defprotocol}, touching without changes preserves protocol identity"
    (tu/init {:auto-keep '#{defprotocol}} 'clj-reload.auto-keep-protocol)
    (let [ns         (find-ns 'clj-reload.auto-keep-protocol)
          proto      @(ns-resolve ns 'IAutoProto)
          method     @(ns-resolve ns '-auto-method)
          rec-inline @(ns-resolve ns 'rec-inline)
          rec-ext-p  @(ns-resolve ns 'rec-extend-proto)
          rec-ext-t  @(ns-resolve ns 'rec-extend-type)
          ext-meta   @(ns-resolve ns 'extend-meta)

          _          (tu/touch 'clj-reload.auto-keep-protocol)
          _          (tu/reload)

          ns'         (find-ns 'clj-reload.auto-keep-protocol)
          proto'      @(ns-resolve ns' 'IAutoProto)
          method'     @(ns-resolve ns' '-auto-method)
          rec-inline' @(ns-resolve ns' 'rec-inline)
          rec-ext-p'  @(ns-resolve ns' 'rec-extend-proto)
          rec-ext-t'  @(ns-resolve ns' 'rec-extend-type)
          ext-meta'   @(ns-resolve ns' 'extend-meta)]

      ;; Protocol was kept — same :on interface class
      (is (identical? (:on proto) (:on proto')))

      ;; Records got new class (only protocol was auto-kept)
      (is (not (identical? rec-inline rec-inline')))
      (is (not (identical? (class rec-inline) (class rec-inline'))))

      ;; Protocol dispatch still works across old and new instances
      (is (= :rec-inline (method rec-inline)))
      (is (= :rec-inline (method rec-inline')))
      (is (= :rec-inline (method' rec-inline)))
      (is (= :rec-inline (method' rec-inline')))

      (is (= :rec-extend-proto (method' rec-ext-p)))
      (is (= :rec-extend-proto (method' rec-ext-p')))

      (is (= :rec-extend-type (method' rec-ext-t)))
      (is (= :rec-extend-type (method' rec-ext-t')))

      (is (= :extend-meta (method ext-meta)))
      (is (= :extend-meta (method ext-meta')))
      (is (= :extend-meta (method' ext-meta)))
      (is (= :extend-meta (method' ext-meta'))))))

(deftest auto-keep-protocol-changed-test
  (testing "With :auto-keep, changing protocol definition creates new protocol"
    (tu/init {:auto-keep '#{defprotocol}} 'clj-reload.auto-keep-protocol)
    (let [ns    (find-ns 'clj-reload.auto-keep-protocol)
          proto @(ns-resolve ns 'IAutoProto)]
      ;; Change protocol definition — add a second method
      (tu/with-changed 'clj-reload.auto-keep-protocol
        "(ns clj-reload.auto-keep-protocol)

(defprotocol IAutoProto
  :extend-via-metadata true
  (-auto-method [_])
  (-extra-method [_]))

(defrecord RecInline []
  IAutoProto
  (-auto-method [_] :rec-inline)
  (-extra-method [_] :extra))

(def rec-inline (RecInline.))

(defrecord RecExtendProto [])
(def rec-extend-proto (RecExtendProto.))
(extend-protocol IAutoProto
  RecExtendProto
  (-auto-method [_] :rec-extend-proto)
  (-extra-method [_] :extra))

(defrecord RecExtendType [])
(def rec-extend-type (RecExtendType.))
(extend-type RecExtendType
  IAutoProto
  (-auto-method [_] :rec-extend-type)
  (-extra-method [_] :extra))

(def extend-meta
  ^{'clj-reload.auto-keep-protocol/-auto-method (fn [_] :extend-meta)
    'clj-reload.auto-keep-protocol/-extra-method (fn [_] :extra)} [])"
        (tu/reload)
        (let [ns'    (find-ns 'clj-reload.auto-keep-protocol)
              proto' @(ns-resolve ns' 'IAutoProto)]
          ;; Protocol changed — should get NEW protocol, not kept
          (is (not (identical? (:on proto) (:on proto'))))
          ;; New method should work
          (let [extra @(ns-resolve ns' '-extra-method)]
            (is (= :extra (extra @(ns-resolve ns' 'rec-inline))))))))))

(deftest auto-keep-protocol-no-config-test
  (testing "Without :auto-keep, bare protocol gets new identity on touch"
    (tu/init 'clj-reload.auto-keep-protocol)
    (let [ns    (find-ns 'clj-reload.auto-keep-protocol)
          proto @(ns-resolve ns 'IAutoProto)
          _     (tu/touch 'clj-reload.auto-keep-protocol)
          _     (tu/reload)
          ns'   (find-ns 'clj-reload.auto-keep-protocol)
          proto' @(ns-resolve ns' 'IAutoProto)]
      ;; Without auto-keep, protocol gets new JVM interface on reload
      (is (not (identical? (:on proto) (:on proto')))))))

;; ============================================================
;; Record auto-keep
;; ============================================================

(deftest auto-keep-record-touch-test
  (testing "With :auto-keep #{defrecord}, touching preserves record class identity"
    (tu/init {:auto-keep '#{defrecord}} 'clj-reload.auto-keep-record)
    (let [ns          (find-ns 'clj-reload.auto-keep-record)
          rec-new     @(ns-resolve ns 'record-new)
          rec-factory @(ns-resolve ns 'record-factory)
          rec-map     @(ns-resolve ns 'record-map-factory)

          _           (tu/touch 'clj-reload.auto-keep-record)
          _           (tu/reload)

          ns'          (find-ns 'clj-reload.auto-keep-record)
          rec-new'     @(ns-resolve ns' 'record-new)
          rec-factory' @(ns-resolve ns' 'record-factory)
          rec-map'     @(ns-resolve ns' 'record-map-factory)]

      ;; Class identity preserved
      (is (identical? (class rec-new) (class rec-new')))
      (is (identical? (class rec-factory) (class rec-factory')))
      (is (identical? (class rec-map) (class rec-map')))

      ;; Instances are not identical (new instances from factory)
      (is (not (identical? rec-new rec-new')))

      ;; But they are equal (same class, same fields)
      (is (= rec-new rec-new'))
      (is (= rec-factory rec-factory'))
      (is (= rec-map rec-map')))))

(deftest auto-keep-record-changed-test
  (testing "With :auto-keep, changing record definition creates new class"
    (tu/init {:auto-keep '#{defrecord}} 'clj-reload.auto-keep-record)
    (let [ns      (find-ns 'clj-reload.auto-keep-record)
          rec-new @(ns-resolve ns 'record-new)]
      (tu/with-changed 'clj-reload.auto-keep-record
        "(ns clj-reload.auto-keep-record)

(defrecord AutoRecord [t extra-field])

(def record-new (AutoRecord. 0 :new))
(def record-factory (->AutoRecord 0 :new))
(def record-map-factory (map->AutoRecord {:t 0 :extra-field :new}))"
        (tu/reload)
        (let [ns'      (find-ns 'clj-reload.auto-keep-record)
              rec-new' @(ns-resolve ns' 'record-new)]
          ;; Changed definition — new class
          (is (not (identical? (class rec-new) (class rec-new')))))))))

(deftest auto-keep-record-no-config-test
  (testing "Without :auto-keep, bare record gets new class on touch"
    (tu/init 'clj-reload.auto-keep-record)
    (let [ns      (find-ns 'clj-reload.auto-keep-record)
          rec-new @(ns-resolve ns 'record-new)
          _       (tu/touch 'clj-reload.auto-keep-record)
          _       (tu/reload)
          ns'     (find-ns 'clj-reload.auto-keep-record)
          rec-new' @(ns-resolve ns' 'record-new)]
      ;; Without auto-keep, gets new class
      (is (not (identical? (class rec-new) (class rec-new')))))))

;; ============================================================
;; Type auto-keep
;; ============================================================

(deftest auto-keep-type-touch-test
  (testing "With :auto-keep #{deftype}, touching preserves type class identity"
    (tu/init {:auto-keep '#{deftype}} 'clj-reload.auto-keep-type)
    (let [ns           (find-ns 'clj-reload.auto-keep-type)
          type-new     @(ns-resolve ns 'type-new)
          type-factory @(ns-resolve ns 'type-factory)

          _            (tu/touch 'clj-reload.auto-keep-type)
          _            (tu/reload)

          ns'           (find-ns 'clj-reload.auto-keep-type)
          type-new'     @(ns-resolve ns' 'type-new)
          type-factory' @(ns-resolve ns' 'type-factory)]

      ;; Class identity preserved
      (is (identical? (class type-new) (class type-new')))
      (is (identical? (class type-factory) (class type-factory')))

      ;; Instances are not identical
      (is (not (identical? type-new type-new')))

      ;; But they are equal
      (is (= type-new type-new'))
      (is (= type-factory type-factory')))))

(deftest auto-keep-type-changed-test
  (testing "With :auto-keep, changing type definition creates new class"
    (tu/init {:auto-keep '#{deftype}} 'clj-reload.auto-keep-type)
    (let [ns       (find-ns 'clj-reload.auto-keep-type)
          type-new @(ns-resolve ns 'type-new)]
      (tu/with-changed 'clj-reload.auto-keep-type
        "(ns clj-reload.auto-keep-type)

(deftype AutoType [t extra]
  java.lang.Object
  (equals [_ o]
    (and (instance? AutoType o)
      (= (.-t ^AutoType o) t)
      (= (.-extra ^AutoType o) extra))))

(def type-new (AutoType. 0 :new))
(def type-factory (->AutoType 0 :new))"
        (tu/reload)
        (let [ns'       (find-ns 'clj-reload.auto-keep-type)
              type-new' @(ns-resolve ns' 'type-new)]
          ;; Changed definition — new class
          (is (not (identical? (class type-new) (class type-new')))))))))

(deftest auto-keep-type-no-config-test
  (testing "Without :auto-keep, bare type gets new class on touch"
    (tu/init 'clj-reload.auto-keep-type)
    (let [ns       (find-ns 'clj-reload.auto-keep-type)
          type-new @(ns-resolve ns 'type-new)
          _        (tu/touch 'clj-reload.auto-keep-type)
          _        (tu/reload)
          ns'      (find-ns 'clj-reload.auto-keep-type)
          type-new' @(ns-resolve ns' 'type-new)]
      ;; Without auto-keep, gets new class
      (is (not (identical? (class type-new) (class type-new')))))))

;; ============================================================
;; Combined auto-keep
;; ============================================================

(deftest auto-keep-combined-test
  (testing "Multiple form tags in :auto-keep all work together"
    (tu/init {:auto-keep '#{defprotocol defrecord deftype}}
      'clj-reload.auto-keep-protocol
      'clj-reload.auto-keep-record
      'clj-reload.auto-keep-type)
    (let [proto-ns   (find-ns 'clj-reload.auto-keep-protocol)
          record-ns  (find-ns 'clj-reload.auto-keep-record)
          type-ns    (find-ns 'clj-reload.auto-keep-type)
          proto      @(ns-resolve proto-ns 'IAutoProto)
          rec-new    @(ns-resolve record-ns 'record-new)
          type-new   @(ns-resolve type-ns 'type-new)

          _          (tu/touch 'clj-reload.auto-keep-protocol)
          _          (tu/touch 'clj-reload.auto-keep-record)
          _          (tu/touch 'clj-reload.auto-keep-type)
          _          (tu/reload)

          proto-ns'  (find-ns 'clj-reload.auto-keep-protocol)
          record-ns' (find-ns 'clj-reload.auto-keep-record)
          type-ns'   (find-ns 'clj-reload.auto-keep-type)
          proto'     @(ns-resolve proto-ns' 'IAutoProto)
          rec-new'   @(ns-resolve record-ns' 'record-new)
          type-new'  @(ns-resolve type-ns' 'type-new)]

      ;; All three preserved
      (is (identical? (:on proto) (:on proto')))
      (is (identical? (class rec-new) (class rec-new')))
      (is (identical? (class type-new) (class type-new'))))))

(deftest auto-keep-double-reload-test
  (testing "Auto-keep survives consecutive reloads"
    (tu/init {:auto-keep '#{defprotocol defrecord}}
      'clj-reload.auto-keep-protocol
      'clj-reload.auto-keep-record)
    (let [proto-ns  (find-ns 'clj-reload.auto-keep-protocol)
          record-ns (find-ns 'clj-reload.auto-keep-record)
          proto     @(ns-resolve proto-ns 'IAutoProto)
          rec-new   @(ns-resolve record-ns 'record-new)

          ;; First reload
          _         (tu/touch 'clj-reload.auto-keep-protocol)
          _         (tu/touch 'clj-reload.auto-keep-record)
          _         (tu/reload)

          proto-ns'  (find-ns 'clj-reload.auto-keep-protocol)
          record-ns' (find-ns 'clj-reload.auto-keep-record)
          proto'     @(ns-resolve proto-ns' 'IAutoProto)
          rec-new'   @(ns-resolve record-ns' 'record-new)

          ;; Second reload
          _         (tu/touch 'clj-reload.auto-keep-protocol)
          _         (tu/touch 'clj-reload.auto-keep-record)
          _         (tu/reload)

          proto-ns''  (find-ns 'clj-reload.auto-keep-protocol)
          record-ns'' (find-ns 'clj-reload.auto-keep-record)
          proto''     @(ns-resolve proto-ns'' 'IAutoProto)
          rec-new''   @(ns-resolve record-ns'' 'record-new)]

      ;; Same identity through both reloads
      (is (identical? (:on proto) (:on proto')))
      (is (identical? (:on proto) (:on proto'')))
      (is (identical? (class rec-new) (class rec-new')))
      (is (identical? (class rec-new) (class rec-new''))))))

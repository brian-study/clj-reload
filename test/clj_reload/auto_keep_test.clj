(ns clj-reload.auto-keep-test
  (:require
    [clojure.test :refer [is deftest testing use-fixtures]]
    [clj-reload.core :as reload]
    [clj-reload.keep :as keep]
    [clj-reload.test-util :as tu]))

(defn wrap-test [f]
  (binding [tu/*dir* "fixtures/auto_keep_test"]
    (tu/reset
      '[clj-reload.auto-keep-protocol
        clj-reload.auto-keep-record
        clj-reload.auto-keep-type
        clj-reload.auto-keep-upstream
        clj-reload.auto-keep-downstream
        clj-reload.auto-keep-proto-and-record])
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

;; ============================================================
;; Adversarial: Transitive reload (dependency change)
;; ============================================================

(deftest auto-keep-transitive-reload-test
  (testing "Protocol survives when reloaded transitively due to dependency change"
    (tu/init {:auto-keep '#{defprotocol}}
      'clj-reload.auto-keep-downstream)
    (let [ns     (find-ns 'clj-reload.auto-keep-downstream)
          proto  @(ns-resolve ns 'IDownstreamProto)
          method @(ns-resolve ns '-downstream-method)
          rec    @(ns-resolve ns 'downstream-rec)

          ;; Touch the UPSTREAM dependency — downstream gets transitively reloaded
          _      (tu/touch 'clj-reload.auto-keep-upstream)
          _      (tu/reload)

          ns'     (find-ns 'clj-reload.auto-keep-downstream)
          proto'  @(ns-resolve ns' 'IDownstreamProto)
          method' @(ns-resolve ns' '-downstream-method)
          rec'    @(ns-resolve ns' 'downstream-rec)]

      ;; Protocol should be kept even though reload was transitive
      (is (identical? (:on proto) (:on proto'))
          "Protocol identity should survive transitive reload")

      ;; Old method var should still dispatch on new record instances
      (is (= :downstream (method rec'))
          "Old method var should dispatch on new instances after transitive reload")

      ;; New method var should dispatch on old record instances
      (is (= :downstream (method' rec))
          "New method var should dispatch on old instances after transitive reload"))))

;; ============================================================
;; Adversarial: Protocol removed from file
;; ============================================================

(deftest auto-keep-protocol-removed-test
  (testing "When auto-kept protocol is REMOVED from file, it should not survive"
    (tu/init {:auto-keep '#{defprotocol}} 'clj-reload.auto-keep-protocol)
    (let [ns    (find-ns 'clj-reload.auto-keep-protocol)
          proto @(ns-resolve ns 'IAutoProto)]
      ;; Remove the protocol entirely from the file
      (tu/with-changed 'clj-reload.auto-keep-protocol
        "(ns clj-reload.auto-keep-protocol)

(def just-a-var 42)"
        (tu/reload)
        (let [ns' (find-ns 'clj-reload.auto-keep-protocol)]
          ;; Protocol should NOT be resolvable in the namespace
          (is (nil? (ns-resolve ns' 'IAutoProto))
              "Removed protocol should not be resolvable after reload"))))))

;; ============================================================
;; Adversarial: Protocol renamed
;; ============================================================

(deftest auto-keep-protocol-renamed-test
  (testing "Renaming auto-kept protocol creates new protocol, old name gone"
    (tu/init {:auto-keep '#{defprotocol}} 'clj-reload.auto-keep-protocol)
    (let [ns    (find-ns 'clj-reload.auto-keep-protocol)
          proto @(ns-resolve ns 'IAutoProto)]
      (tu/with-changed 'clj-reload.auto-keep-protocol
        "(ns clj-reload.auto-keep-protocol)

(defprotocol IRenamed
  :extend-via-metadata true
  (-auto-method [_]))

(defrecord RecInline []
  IRenamed
  (-auto-method [_] :rec-inline))

(def rec-inline (RecInline.))

(defrecord RecExtendProto [])
(def rec-extend-proto (RecExtendProto.))
(extend-protocol IRenamed
  RecExtendProto
  (-auto-method [_] :rec-extend-proto))

(defrecord RecExtendType [])
(def rec-extend-type (RecExtendType.))
(extend-type RecExtendType
  IRenamed
  (-auto-method [_] :rec-extend-type))

(def extend-meta
  ^{'clj-reload.auto-keep-protocol/-auto-method (fn [_] :extend-meta)} [])"
        (tu/reload)
        (let [ns'       (find-ns 'clj-reload.auto-keep-protocol)
              renamed   @(ns-resolve ns' 'IRenamed)]
          ;; Old name should not be resolvable
          (is (nil? (ns-resolve ns' 'IAutoProto))
              "Old protocol name should not be resolvable after rename")
          ;; New name should be a new protocol, not the old one
          (is (some? renamed)
              "Renamed protocol should be resolvable")
          (is (not (identical? (:on proto) (:on renamed)))
              "Renamed protocol should have new identity"))))))

;; ============================================================
;; Adversarial: Protocol + Record both auto-kept
;; ============================================================

(deftest auto-keep-protocol-and-record-combined-test
  (testing "Both protocol AND record auto-kept preserves full dispatch chain"
    (tu/init {:auto-keep '#{defprotocol defrecord}}
      'clj-reload.auto-keep-proto-and-record)
    (let [ns     (find-ns 'clj-reload.auto-keep-proto-and-record)
          proto  @(ns-resolve ns 'ICombined)
          method @(ns-resolve ns '-combined-method)
          crec   @(ns-resolve ns 'combined-rec)
          erec   @(ns-resolve ns 'ext-rec)

          _      (tu/touch 'clj-reload.auto-keep-proto-and-record)
          _      (tu/reload)

          ns'     (find-ns 'clj-reload.auto-keep-proto-and-record)
          proto'  @(ns-resolve ns' 'ICombined)
          method' @(ns-resolve ns' '-combined-method)
          crec'   @(ns-resolve ns' 'combined-rec)
          erec'   @(ns-resolve ns' 'ext-rec)]

      ;; Both protocol and record classes preserved
      (is (identical? (:on proto) (:on proto'))
          "Protocol identity should be preserved")
      (is (identical? (class crec) (class crec'))
          "Record class should be preserved when both are auto-kept")

      ;; Inline implementation on old instance with new method var
      (is (= :inline-impl (method' crec))
          "New method var should dispatch inline impl on old instance")

      ;; Inline implementation on new instance with old method var
      (is (= :inline-impl (method crec'))
          "Old method var should dispatch inline impl on new instance")

      ;; extend-protocol implementation survives
      (is (= :ext-impl (method' erec'))
          "extend-protocol impl should work after reload")
      (is (= :ext-impl (method erec'))
          "Old method should dispatch extend-protocol impl on new instances"))))

;; ============================================================
;; Adversarial: Record auto-kept but protocol NOT — satisfies? check
;; ============================================================

(deftest auto-keep-record-only-satisfies-test
  (testing "Record auto-kept, protocol NOT — old instances and satisfies?"
    (tu/init {:auto-keep '#{defrecord}}
      'clj-reload.auto-keep-proto-and-record)
    (let [ns     (find-ns 'clj-reload.auto-keep-proto-and-record)
          proto  @(ns-resolve ns 'ICombined)
          method @(ns-resolve ns '-combined-method)
          crec   @(ns-resolve ns 'combined-rec)

          _      (tu/touch 'clj-reload.auto-keep-proto-and-record)
          _      (tu/reload)

          ns'     (find-ns 'clj-reload.auto-keep-proto-and-record)
          proto'  @(ns-resolve ns' 'ICombined)
          method' @(ns-resolve ns' '-combined-method)
          crec'   @(ns-resolve ns' 'combined-rec)]

      ;; Protocol gets new identity (not auto-kept)
      (is (not (identical? (:on proto) (:on proto')))
          "Protocol should get new identity when not auto-kept")

      ;; Record class preserved (auto-kept)
      (is (identical? (class crec) (class crec'))
          "Record class should be preserved")

      ;; New method on old instance -- this is the critical case.
      ;; Record class is the same, but protocol is new.
      ;; Inline impl was compiled against OLD protocol interface.
      ;; New protocol has a DIFFERENT JVM interface.
      ;; Does dispatch work?
      ;; The old record's inline impl is for the OLD interface, so calling
      ;; through the new protocol's method var should fail with
      ;; "No implementation of method" or fall through to :default.
      ;;
      ;; This documents the EXPECTED BEHAVIOR: when only record is kept
      ;; but protocol is NOT kept, inline impls on OLD instances break.
      ;; Users should auto-keep protocols too, or use extend-protocol.
      (is (thrown? IllegalArgumentException (method' crec))
          "Old instance's inline impl should not satisfy new protocol"))))

;; ============================================================
;; Adversarial: Change then revert (form goes back to original)
;; ============================================================

(deftest auto-keep-change-then-revert-test
  (testing "Protocol changed then reverted back to original form is still kept"
    (tu/init {:auto-keep '#{defprotocol}} 'clj-reload.auto-keep-protocol)
    (let [ns    (find-ns 'clj-reload.auto-keep-protocol)
          proto @(ns-resolve ns 'IAutoProto)]

      ;; First: change the protocol (adds extra method)
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
        (let [ns-mid    (find-ns 'clj-reload.auto-keep-protocol)
              proto-mid @(ns-resolve ns-mid 'IAutoProto)]
          ;; Changed — new protocol
          (is (not (identical? (:on proto) (:on proto-mid)))
              "Changed protocol should have new identity")))

      ;; Now the with-changed block restores the original file.
      ;; The file now has the original form again, but the current state
      ;; has the CHANGED form. So carry-keeps will set :prev-form to the
      ;; changed form, and :form to the original — they differ, so keep
      ;; is skipped and we get ANOTHER new protocol.
      (tu/reload)
      (let [ns'    (find-ns 'clj-reload.auto-keep-protocol)
            proto' @(ns-resolve ns' 'IAutoProto)]
        ;; Reverted form differs from mid-form, so a new protocol is expected
        (is (not (identical? (:on proto) (:on proto')))
            "Reverted protocol should have new identity (prev-form was the changed version)")))))

;; ============================================================
;; Adversarial: First load (no previous state)
;; ============================================================

(deftest auto-keep-first-load-test
  (testing "Auto-keep on first load (no previous state) should work normally"
    ;; Init without requiring the namespace first
    (tu/init {:auto-keep '#{defprotocol defrecord}}
      'clj-reload.auto-keep-protocol)
    (let [ns    (find-ns 'clj-reload.auto-keep-protocol)
          proto @(ns-resolve ns 'IAutoProto)]
      ;; Should have loaded successfully
      (is (some? proto)
          "Protocol should be loaded on first init")
      (is (some? (:on proto))
          "Protocol should have :on class on first init"))))

;; ============================================================
;; Adversarial: Annotation + auto-keep coexistence
;; ============================================================

(deftest auto-keep-with-annotation-test
  (testing "Form with ^:clj-reload/keep AND auto-keep both active"
    ;; Use a file that has the annotation, but also enable auto-keep
    (tu/init {:auto-keep '#{defprotocol}}
      'clj-reload.auto-keep-protocol)
    (let [ns     (find-ns 'clj-reload.auto-keep-protocol)
          proto  @(ns-resolve ns 'IAutoProto)
          method @(ns-resolve ns '-auto-method)]

      ;; Replace file content to add ^:clj-reload/keep annotation on top of auto-keep
      (tu/with-changed 'clj-reload.auto-keep-protocol
        "(ns clj-reload.auto-keep-protocol)

^:clj-reload/keep
(defprotocol IAutoProto
  :extend-via-metadata true
  (-auto-method [_]))

(defrecord RecInline []
  IAutoProto
  (-auto-method [_] :rec-inline))

(def rec-inline (RecInline.))

(defrecord RecExtendProto [])
(def rec-extend-proto (RecExtendProto.))
(extend-protocol IAutoProto
  RecExtendProto
  (-auto-method [_] :rec-extend-proto))

(defrecord RecExtendType [])
(def rec-extend-type (RecExtendType.))
(extend-type RecExtendType
  IAutoProto
  (-auto-method [_] :rec-extend-type))

(def extend-meta
  ^{'clj-reload.auto-keep-protocol/-auto-method (fn [_] :extend-meta)} [])"
        (tu/reload)
        (let [ns'     (find-ns 'clj-reload.auto-keep-protocol)
              proto'  @(ns-resolve ns' 'IAutoProto)
              method' @(ns-resolve ns' '-auto-method)]
          ;; The form changed (added annotation metadata), but = ignores metadata
          ;; so form-unchanged? returns true. Protocol should be kept.
          (is (identical? (:on proto) (:on proto'))
              "Adding annotation should not change form identity (metadata ignored by =)")

          ;; Dispatch should still work
          (is (= :rec-inline (method' @(ns-resolve ns' 'rec-inline)))
              "Method dispatch should work after adding annotation"))))))

;; ============================================================
;; Adversarial: Error during reload preserves auto-keep state
;; ============================================================

(deftest auto-keep-error-during-reload-test
  (testing "Auto-keep state survives a failed reload and works on retry"
    (tu/init {:auto-keep '#{defprotocol}} 'clj-reload.auto-keep-protocol)
    (let [ns    (find-ns 'clj-reload.auto-keep-protocol)
          proto @(ns-resolve ns 'IAutoProto)]

      ;; Break the file
      (tu/with-changed 'clj-reload.auto-keep-protocol
        "(ns clj-reload.auto-keep-protocol)
(defprotocol IAutoProto
  :extend-via-metadata true
  (-auto-method [_]))
oops-undefined-symbol!"
        (is (thrown? Exception (tu/reload))))

      ;; File is restored by with-changed. Reload should work now.
      (tu/reload)
      (let [ns'    (find-ns 'clj-reload.auto-keep-protocol)
            proto' @(ns-resolve ns' 'IAutoProto)]
        ;; Protocol form hasn't changed (same as original), so should be kept
        (is (identical? (:on proto) (:on proto'))
            "Protocol identity should survive error+retry reload cycle")))))

;; ============================================================
;; Adversarial: Protocol method reordering
;; ============================================================

(deftest auto-keep-protocol-docstring-change-test
  (testing "Adding a docstring to protocol changes the form and creates new protocol"
    (tu/init {:auto-keep '#{defprotocol}}
      'clj-reload.auto-keep-proto-and-record)
    (let [ns    (find-ns 'clj-reload.auto-keep-proto-and-record)
          proto @(ns-resolve ns 'ICombined)]
      ;; Add a docstring — this changes the form
      (tu/with-changed 'clj-reload.auto-keep-proto-and-record
        "(ns clj-reload.auto-keep-proto-and-record)

(defprotocol ICombined
  \"A combined protocol with a docstring\"
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
  (-combined-method [_] :ext-impl))"
        (tu/reload)
        (let [ns'    (find-ns 'clj-reload.auto-keep-proto-and-record)
              proto' @(ns-resolve ns' 'ICombined)]
          (is (not (identical? (:on proto) (:on proto')))
              "Adding docstring to protocol should create new protocol"))))))

;; ============================================================
;; Adversarial: Triple reload — change, revert, touch
;; ============================================================

(deftest auto-keep-change-revert-touch-test
  (testing "Change-revert-touch sequence: identity should be kept on final touch"
    (tu/init {:auto-keep '#{defrecord}} 'clj-reload.auto-keep-record)
    (let [ns      (find-ns 'clj-reload.auto-keep-record)
          rec-new @(ns-resolve ns 'record-new)]

      ;; Step 1: Change the record (add field)
      (tu/with-changed 'clj-reload.auto-keep-record
        "(ns clj-reload.auto-keep-record)

(defrecord AutoRecord [t extra])

(def record-new (AutoRecord. 0 :x))
(def record-factory (->AutoRecord 0 :x))
(def record-map-factory (map->AutoRecord {:t 0 :extra :x}))"
        (tu/reload)
        (let [ns-mid      (find-ns 'clj-reload.auto-keep-record)
              rec-mid @(ns-resolve ns-mid 'record-new)]
          ;; Changed — new class
          (is (not (identical? (class rec-new) (class rec-mid)))
              "Changed record should have new class")))

      ;; Step 2: File reverted by with-changed. Reload picks up the revert.
      (tu/reload)
      (let [ns-rev      (find-ns 'clj-reload.auto-keep-record)
            rec-rev @(ns-resolve ns-rev 'record-new)]
        ;; Reverted form differs from mid-form, so new class
        (is (not (identical? (class rec-new) (class rec-rev)))
            "Reverted record should have new class (prev was changed form)")

        ;; Step 3: Touch without changes — should be kept now
        (tu/touch 'clj-reload.auto-keep-record)
        (tu/reload)
        (let [ns-final      (find-ns 'clj-reload.auto-keep-record)
              rec-final @(ns-resolve ns-final 'record-new)]
          (is (identical? (class rec-rev) (class rec-final))
              "Touch after revert should preserve class identity"))))))

;; ============================================================
;; Adversarial: Old instance protocol dispatch (the real-world bug)
;; ============================================================

(deftest auto-keep-old-instance-dispatch-test
  (testing "The actual bug: old record instance + protocol method after reload"
    (tu/init {:auto-keep '#{defprotocol}} 'clj-reload.auto-keep-protocol)
    (let [ns           (find-ns 'clj-reload.auto-keep-protocol)
          method       @(ns-resolve ns '-auto-method)
          rec-inline   @(ns-resolve ns 'rec-inline)
          rec-ext-p    @(ns-resolve ns 'rec-extend-proto)
          rec-ext-t    @(ns-resolve ns 'rec-extend-type)
          ext-meta     @(ns-resolve ns 'extend-meta)]

      ;; Simulate: user has references to old instances in their REPL
      ;; Touch and reload (common dev workflow)
      (tu/touch 'clj-reload.auto-keep-protocol)
      (tu/reload)

      (let [ns'     (find-ns 'clj-reload.auto-keep-protocol)
            method' @(ns-resolve ns' '-auto-method)]

        ;; The critical test: can we call the NEW method var on OLD instances?
        ;; Without auto-keep, this would throw "No implementation of method"
        ;; for inline impls (because the old class implements the old interface).
        ;; With auto-keep, the protocol interface is the same, so it should work.
        (is (= :rec-inline (method' rec-inline))
            "NEW method var on OLD inline-impl instance should work")

        ;; Old method var on old instances should also work
        (is (= :rec-inline (method rec-inline))
            "OLD method var on OLD inline-impl instance should work")

        ;; extend-protocol on old instances
        (is (= :rec-extend-proto (method' rec-ext-p))
            "NEW method on OLD extend-protocol instance should work")

        ;; extend-type on old instances
        (is (= :rec-extend-type (method' rec-ext-t))
            "NEW method on OLD extend-type instance should work")

        ;; metadata-based extension on old instances
        (is (= :extend-meta (method' ext-meta))
            "NEW method on OLD metadata-extended instance should work")))))

;; ============================================================
;; Adversarial: Error BEFORE protocol definition in file
;; ============================================================

(deftest auto-keep-error-before-protocol-test
  (testing "Error before protocol definition — protocol not defined in partial load"
    (tu/init {:auto-keep '#{defprotocol}} 'clj-reload.auto-keep-protocol)
    (let [ns    (find-ns 'clj-reload.auto-keep-protocol)
          proto @(ns-resolve ns 'IAutoProto)]

      ;; Put the error BEFORE the protocol definition
      (tu/with-changed 'clj-reload.auto-keep-protocol
        "(ns clj-reload.auto-keep-protocol)
oops-undefined-symbol!
(defprotocol IAutoProto
  :extend-via-metadata true
  (-auto-method [_]))"
        (is (thrown? Exception (tu/reload))))

      ;; File restored. Reload should work.
      ;; The critical question: is the protocol identity preserved?
      ;; After the failed load, the namespace was partially loaded but the
      ;; protocol wasn't defined (error came before it). On retry, resolve-keeps
      ;; needs to find the protocol var. But the ns was unloaded during the
      ;; failed reload attempt, and the protocol var no longer exists.
      ;; The keep entry from carry-keeps still has the old resolved vars from
      ;; the unload phase of the failed attempt. Does the re-unload on retry
      ;; find the var?
      (tu/reload)
      (let [ns'    (find-ns 'clj-reload.auto-keep-protocol)
            proto' @(ns-resolve ns' 'IAutoProto)]
        ;; The protocol should still load successfully
        (is (some? proto')
            "Protocol should be loadable after error recovery")
        ;; Protocol identity IS preserved because:
        ;; 1. During the first failed reload, resolve-keeps ran BEFORE ns-unload
        ;;    and captured the Var references from the original successful load
        ;; 2. The captured Vars survived in the state (deep-merge preserves them)
        ;; 3. On retry, the stale resolved keeps from the first unload are reused
        ;;    to patch the file, restoring the original protocol class
        (is (identical? (:on proto) (:on proto'))
            "Protocol identity preserved even when error was before definition"))))

  (testing "Error after protocol definition — protocol identity preserved on retry"
    (tu/init {:auto-keep '#{defprotocol}} 'clj-reload.auto-keep-protocol)
    (let [ns    (find-ns 'clj-reload.auto-keep-protocol)
          proto @(ns-resolve ns 'IAutoProto)]

      ;; Error AFTER the protocol is defined
      (tu/with-changed 'clj-reload.auto-keep-protocol
        "(ns clj-reload.auto-keep-protocol)

(defprotocol IAutoProto
  :extend-via-metadata true
  (-auto-method [_]))

oops-undefined-symbol!"
        (is (thrown? Exception (tu/reload))))

      ;; File restored. Retry.
      (tu/reload)
      (let [ns'    (find-ns 'clj-reload.auto-keep-protocol)
            proto' @(ns-resolve ns' 'IAutoProto)]
        (is (some? proto')
            "Protocol should be loadable after error recovery")
        ;; The protocol was defined during the partial load, so resolve-keeps
        ;; should have captured it. On retry, the keep should work.
        (is (identical? (:on proto) (:on proto'))
            "Protocol identity preserved when error occurred after definition")))))

;; ============================================================
;; Adversarial: Stale keep entry not carried when form removed
;; ============================================================

(deftest auto-keep-stale-keep-not-patched-test
  (testing "Removed auto-kept form's stale keep entry does not corrupt new file"
    (tu/init {:auto-keep '#{defprotocol defrecord}}
      'clj-reload.auto-keep-proto-and-record)
    (let [ns     (find-ns 'clj-reload.auto-keep-proto-and-record)
          proto  @(ns-resolve ns 'ICombined)
          method @(ns-resolve ns '-combined-method)
          crec   @(ns-resolve ns 'combined-rec)]

      ;; Remove the record, keep only the protocol
      (tu/with-changed 'clj-reload.auto-keep-proto-and-record
        "(ns clj-reload.auto-keep-proto-and-record)

(defprotocol ICombined
  :extend-via-metadata true
  (-combined-method [_]))

(def combined-val :no-record-anymore)"
        (tu/reload)
        (let [ns'    (find-ns 'clj-reload.auto-keep-proto-and-record)
              proto' @(ns-resolve ns' 'ICombined)
              val'   @(ns-resolve ns' 'combined-val)]
          ;; Protocol kept
          (is (identical? (:on proto) (:on proto'))
              "Protocol should be kept")
          ;; Record gone
          (is (nil? (ns-resolve ns' 'CombinedRec))
              "Removed record should not be resolvable")
          (is (nil? (ns-resolve ns' '->CombinedRec))
              "Removed record constructor should not be resolvable")
          ;; New var works
          (is (= :no-record-anymore val')
              "New var should be defined correctly"))))))

;; ============================================================
;; Adversarial: form-unchanged? unit test — nil prev-form
;; ============================================================

(deftest form-unchanged-nil-prev-form-test
  (testing "form-unchanged? with nil prev-form (first scan) returns true"
    (is (true? (clj-reload.keep/form-unchanged?
                 {:form '(defprotocol Foo (-bar [_]))
                  :prev-form nil}))
        "nil prev-form should be treated as unchanged"))

  (testing "form-unchanged? with matching forms returns true"
    (is (true? (clj-reload.keep/form-unchanged?
                 {:form '(defprotocol Foo (-bar [_]))
                  :prev-form '(defprotocol Foo (-bar [_]))}))
        "Identical forms should be unchanged"))

  (testing "form-unchanged? with different forms returns false"
    (is (false? (clj-reload.keep/form-unchanged?
                  {:form '(defprotocol Foo (-bar [_]) (-baz [_]))
                   :prev-form '(defprotocol Foo (-bar [_]))}))
        "Different forms should be detected as changed"))

  (testing "form-unchanged? with absent prev-form key returns true"
    (is (true? (clj-reload.keep/form-unchanged?
                 {:form '(defprotocol Foo (-bar [_]))}))
        "Missing prev-form key should be treated as unchanged (first load)")))

(ns spec-extended.errors
  (:require [clojure.spec :as s]))

(defn throw-spec [spec value]
  (throw (ex-info "value did not conform to spec"
                  {:type ::s/invalid
                   :reason "value did not conform to spec"
                   :spec spec
                   :value value})))

(defmacro catch-errors-valid?
  [spec expr]
  `(try (s/valid? ~spec ~expr)
        (catch Exception e# nil)))

(defmacro only-catch-spec-errors
  [expr]
  `(try ~expr
        (catch Exception e#
          (when-not (= ::invalid (:type (ex-data e#)))
            (throw e#)))))

(defmacro only-catch-invalid
  [expr]
  `(try ~expr
        (catch Exception e#
          (if (= (:type e#) ::s/invalid)
            (throw e#)))))

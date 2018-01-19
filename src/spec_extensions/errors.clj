(ns spec-extensions.errors
  (:require [clojure.spec :as s]))

(defn throw-spec [spec value]
  (throw (ex-info "value did not conform to spec"
                  {:type ::invalid
                   :reason "value did not conform to spec"
                   :spec spec
                   :value value})))

(defmacro catch-errors-valid?
  [spec expr]
  `(try (s/valid? ~spec ~expr)
        (catch Exception e# nil)))

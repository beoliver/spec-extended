(ns spec-extended.core
  "common clojure macros extended to work with specs."
  (:require [clojure.spec :as s]
            [spec-extended.errors :refer [throw-spec
                                          catch-errors-valid?
                                          only-catch-spec-errors]])
  (:refer-clojure :exclude [if-let when-let as->]))

(defn fmap [f x]
  (if-not (s/invalid? x) (f x) x))

(defn nil-when-invalid
  "returns `nil` if x is `:clojure.spec/invalid` else x"
  [x]
  (when-not (s/invalid? x) x))

(defmacro if-let
  "an extension of `clojure.core/if-let`"
  ([spec bindings then]
   `(if-let ~spec ~bindings ~then ::s/invalid))
  ([spec bindings then else]
   (let [form (bindings 0) rhs (bindings 1)]
     `(let [res# ~rhs]
        (if (s/valid? ~spec res#)
          (let [~form res#]
            ~then)
          ~else)))))

(defmacro when-let
  "an extension of `clojure.core/when-let`"
  [spec bindings then]
  `(if-let ~spec ~bindings ~then ::s/invalid))

(defmacro conforms->
  "When expr is not `:clojure.spec/invalid`, threads it into the first form (via ->),
  and when that result is not `:clojure.spec/invalid`, through the next etc"
  [expr spec & forms]
  (let [g (gensym)
        steps (map (fn [[step spec]] `(if (s/invalid? ~g) ~g (s/conform ~spec (-> ~g ~step))))
                   (partition 2 forms))]
    `(let [~g (s/conform ~spec ~expr)
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(defmacro conforms->>
  "When expr is not `:clojure.spec/invalid`, threads it into the first form (via ->>),
  and when that result is not `:clojure.spec/invalid`, through the next etc"
  [expr spec & forms]
  (let [g (gensym)
        steps (map (fn [[step spec]] `(if (s/invalid? ~g) ~g (s/conform ~spec (->> ~g ~step))))
                   (partition 2 forms))]
    `(let [~g (s/conform ~spec ~expr)
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(defmacro ^{:private true} assert-args
  [& pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                 (str (first ~'&form) " requires " ~(second pairs) " in " ~'*ns* ":" (:line (meta ~'&form))))))
       ~(let [more (nnext pairs)]
          (when more
            (list* `assert-args more)))))

(defmacro as->
  [expr name spec & forms]
  (assert-args
   (even? (count forms)) "an even number of form pairs")
  (let [steps (map (fn [[step spec]]
                     `(if (s/invalid? ~name) ~name (s/conform ~spec ~step)))
                   (partition 2 forms))]
    `(let [~name (s/conform ~spec ~expr)
           ~@(interleave (repeat name) (butlast steps))]
       ~(if (empty? steps)
          name
          (last steps)))))

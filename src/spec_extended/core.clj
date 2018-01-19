(ns spec-extensded.core
  "common clojure macros extended to work with specs."
  (:require [clojure.spec :as s]
            [spec-extended.errors :refer [throw-spec
                                          catch-errors-valid?
                                          only-catch-spec-errors]])
  (:refer-clojure :exclude [if-let when-let some-> some->> as->]))

(declare if-let)
(declare if-lets)
(declare when-let)
(declare when-lets)

(declare spec-when-let!)
(declare spec-when-lets!)

(declare some->)

(declare spec-some->!)
(declare spec-some->>)
(declare spec-some->>!)
(declare spec->)
(declare spec->!)
(declare spec->>)
(declare spec->>!)

(defmacro if-let
  "an extension of `if-let`. Only executes the `then` when the bound value
   conforms to `spec`.

   (if-let <spec> <bindings> <then>)
   (if-let <spec> <bindings> <then> <else>)

   like if-let, <bindings> is of the form [<var> <expr>]

   examples
   (if-let even? [x 10] (println x))
   (if-let ::my-spec [x (some-fn)] (some-other-fn x) :else-branch)
   (if-let nil? [x (println 1)] :printed :didnt-print)
  "
  ([spec bindings then]
   `(if-let ~spec ~bindings ~then nil))
  ([spec bindings then else]
   (let [form (bindings 0) rhs (bindings 1)]
     `(let [res# ~rhs]
        (if (catch-errors-valid? ~spec res#)
          (let [~form res#]
            ~then)
          ~else)))))

(defmacro when-let
  "an extension of `when-let`. Only executes the `then` when the bound value
   conforms to `spec`.

  (when-let <spec> <bindings> <then>)
  (when-let <spec> <bindings> <then> <else>)

  like when-let, <bindings> is of the form [<var> <expr>]

  example
  (when-let even? [x 10] (println x))
  (if-let ::my-spec [x (some-fn)] (some-other-fn x))
  (when-let nil? [x (println 1)] :printed)
  "
  [spec bindings then]
  (let [form (bindings 0) rhs (bindings 1)]
    `(let [res# ~rhs]
       (when (catch-errors-valid? ~spec res#)
         (let [~form res#]
           ~then)))))

(defmacro spec-when-let!
  "an extension of `when-let`. Only executes the `then` when the bound value
   conforms to `spec`. If the value is not `valid?` then an `ex-info` error
   is thrown.

   (spec-when-let! <spec> <bindings> <then>)
   (spec-when-let! <spec> <bindings> <then> <else>)

   like when-let, <bindings> is of the form [<var> <expr>]

   example
   (spec-when-let! even? [x 10] (println x))
   (spec-when-let! ::my-spec [x (some-fn)] (some-other-fn x))
   (spec-when-let! nil? [x (println 1)] :printed)
  "
  ([spec bindings then]
   (let [form (bindings 0) rhs (bindings 1)]
     `(let [res# ~rhs]
        (if (s/valid? ~spec res#)
          (let [~form res#]
            ~then)
          (throw-spec ~spec res#))))))

(defmacro spec-if-let*
  ([bindings then else]
   (let [bindings' (bindings 0)
         spec (bindings 1)
         form (bindings' 0)
         rhs (bindings' 1)]
     `(let [res# ~rhs]
        (if (s/valid? ~spec res#)
          (let [~form res#]
            ~then)
          ~else)))))

(defmacro spec-when-let!*
  ([bindings then else]
   (let [bindings' (bindings 0)
         spec (bindings 1)
         form (bindings' 0)
         rhs (bindings' 1)]
     `(let [res# ~rhs]
        (if (s/valid? ~spec res#)
          (let [~form res#]
            ~then)
          (throw-spec ~spec res#))))))

(defmacro if-lets
  "allows multiple bindings, where bindings take the form
  [[<var-1> <expr-1>] <spec-1> ... [<var-n> <expr-n>] <spec-n>]

  example
  (spec-if-lets [[x 1] odd? [y 2] ::my-spec] (+ x y) :boop)
  "
  ([bindings then]
   `(if-lets ~bindings ~then nil))
  ([bindings then else]
   (if (seq bindings)
     `(spec-if-let* [~(first bindings) ~(second bindings)]
        (if-lets ~(drop 2 bindings) ~then ~else)
        ~else)
     then)))

(defmacro when-lets
  "allows multiple bindings, where bindings take the form
  [[<var-1> <expr-1>] <spec-1> ... [<var-n> <expr-n>] <spec-n>]

  example
  (spec-when-lets [[x 1] odd? [y 2] ::my-spec] (+ x y))
  "
  ([bindings then]
   (if (seq bindings)
     `(spec-if-let* [~(first bindings) ~(second bindings)]
        (when-lets ~(drop 2 bindings) ~then)
        nil)
     then)))

(defmacro spec-when-lets!
  "allows multiple bindings, where bindings take the form
  [[<var-1> <expr-1>] <spec-1> ... [<var-n> <expr-n>] <spec-n>]
  Instead of retuning `nil` if the value is not `valid?` an `ex-info` error is thrown

  example
  (spec-when-lets [[x 1] odd? [y 2] ::my-spec] (+ x y))
  "
  ([bindings then]
   (if (seq bindings)
     `(spec-when-let!* [~(first bindings) ~(second bindings)]
        (spec-when-lets! ~(drop 2 bindings) ~then))
     then)))

(defmacro some->
  "Similar to `some->` but `forms` is a sequence of clauses.

  (some-> <expr>
          <spec> <form>
          <spec> <form>
          ...
          <spec> <form>)

  Each <spec> is treated as a pre-condition.
  When expr conforms to the supplied spec, threads it into the first form (via ->),
  and when that result conforms, through the next etc.

  As soon as a pre-condition fails, the execution is aborted.
  This avoids the possiblity that a `nil` is propogated though the chain.
  If an excption is thrown when persofming a spec check then `nil` is returned,
  however all other errors are caught and re-thrown
  "
  [expr & forms]
  (let [g (gensym)
        steps (map (fn [[spec step]] `(if (try (s/valid? ~spec ~g)
                                               (catch Exception e#
                                                 (throw-spec ~spec ~g)))
                                        (-> ~g ~step)
                                        (throw-spec ~spec ~g)))
                   (partition 2 forms))]
    `(only-catch-spec-errors (let [~g ~expr
                                   ~@(interleave (repeat g) (butlast steps))]
                               ~(if (empty? steps)
                                  g
                                  (last steps))))))

(defmacro some->>
  "Similar to `some->>` but `forms` is a sequence of clauses.

   (some->> <expr>
            <spec> <form>
            <spec> <form>
            ...
            <spec> <form>)

   Each <spec> is treated as a pre-condition.
   When expr conforms to the supplied spec, threads it into the first form (via ->>),
   and when that result conforms, through the next etc
  "
  [expr & forms]
  (let [g (gensym)
        steps (map (fn [[spec step]] `(if (try (s/valid? ~spec ~g)
                                               (catch Exception e#
                                                 (throw-spec ~spec ~g)))
                                        (->> ~g ~step)
                                        (throw-spec ~spec ~g)))
                   (partition 2 forms))]
    `(only-catch-spec-errors (let [~g ~expr
                                   ~@(interleave (repeat g) (butlast steps))]
                               ~(if (empty? steps)
                                  g
                                  (last steps))))))

;;; TODO implement the error handling used in spec-some->

(defmacro spec-some->!
  "Similar to `some->` but `forms` is a sequence of clauses.
   If a spec test is not valid, throws an `ex-info` error

  (spec-some->! <expr>
                <spec-1> <form-1>
                <spec-2> <form-2>
                ...
                <spec-n> <form-n>)

  Each <spec> is treated as a pre-condition.
  When expr conforms to the supplied spec, threads it into the first form (via ->),
  and when that result conforms, through the next etc.
  "
  [expr & forms]
  (let [g (gensym)
        steps (map (fn [[spec step]] `(if (s/valid? ~spec ~g)
                                        (-> ~g ~step)
                                        (throw-spec ~spec ~g)))
                   (partition 2 forms))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(defmacro spec-some->>!
  "Similar to `some->>` but `forms` is a sequence of clauses.
   If a spec test is not valid, throws an `ex-info` error

   (spec-some->>! <expr>
                  <spec-1> <form-1>
                  <spec-2> <form-2>
                  ...
                  <spec-n> <form-n>)

   Each <spec> is treated as a pre-condition.
   When expr conforms to the supplied spec, threads it into the first form (via ->>),
   and when that result conforms, through the next etc
  "
  [expr & forms]
  (let [g (gensym)
        steps (map (fn [[spec step]] `(if (s/valid? ~spec ~g)
                                        (->> ~g ~step)
                                        (throw-spec ~spec ~g)))
                   (partition 2 forms))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(defmacro spec->
  "Similar to `some->` but `forms` is a sequence of clauses.

  (spec-> <expr>
          <pre-post> <form>
          <pre-post> <form>
          ...
          <pre-post> <form>)

  where <pre-post> is anything that contains keys `:pre` and `:post`.
  If a key is not supplied then the test is passed. This allows to only
  run pre/post condtions or a mix of both. An empty map means no tests are run.

  When expr conforms to the supplied spec, threads it into the first form (via ->),
  and when that result conforms, through the next etc"
  [expr & forms]
  (let [g (gensym)
        steps (map (fn [[spec step]] `(clojure.core/if-let [pre# (:pre ~spec)]
                                        (when (s/valid? pre# ~g)
                                          (let [res# (-> ~g ~step)]
                                            (clojure.core/if-let [post# (:post ~spec)]
                                              (when (s/valid? post# res#)
                                                res#)
                                              res#)))
                                        (-> ~g ~step)))
                   (partition 2 forms))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(defmacro spec->!
  "Similar to `some->` but `forms` is a sequence of clauses.
   If a spec test is not valid, throws an `ex-info` error

  (spec->! <expr>
           <pre-post> <form>
           <pre-post> <form>
           ...
           <pre-post> <form>)

  where <pre-post> is anything that contains keys `:pre` and `:post`.
  If a key is not supplied then the test is passed. This allows to only
  run pre/post condtions or a mix of both. An empty map means no tests are run.

  When expr conforms to the supplied spec, threads it into the first form (via ->),
  and when that result conforms, through the next etc"
  [expr & forms]
  (let [g (gensym)
        steps (map (fn [[spec step]] `(clojure.core/if-let [pre# (:pre ~spec)]
                                        (if-not (s/valid? pre# ~g)
                                          (throw-spec ~spec ~g)
                                          (let [res# (-> ~g ~step)]
                                            (clojure.core/if-let [post# (:post ~spec)]
                                              (if-not (s/valid? post# res#)
                                                (throw-spec ~spec res#)
                                                res#)
                                              res#)))
                                        (-> ~g ~step)))
                   (partition 2 forms))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(defmacro spec->>
  "Similar to `some->>` but `forms` is a sequence of clauses.

  (spec->> <expr>
           <pre-post> <form>
           <pre-post> <form>
           ...
           <pre-post> <form>)

  where <pre-post> is anything that contains keys `:pre` and `:post`.
  If a key is not supplied then the test is passed. This allows to only
  run pre/post condtions or a mix of both. An empty map means no tests are run.

  When expr conforms to the supplied spec, threads it into the first form (via ->>),
  and when that result conforms, through the next etc"
  [expr & forms]
  (let [g (gensym)
        steps (map (fn [[spec step]] `(clojure.core/if-let [pre# (:pre ~spec)]
                                        (when (s/valid? pre# ~g)
                                          (let [res# (->> ~g ~step)]
                                            (clojure.core/if-let [post# (:post ~spec)]
                                              (when (s/valid? post# res#)
                                                res#)
                                              res#)))
                                        (->> ~g ~step)))
                   (partition 2 forms))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))


(defmacro spec->>!
  "Similar to `some->>` but `forms` is a sequence of clauses.
   If a spec test is not valid, throws an `ex-info` error

  (spec->>! <expr>
            <pre-post> <form>
            <pre-post> <form>
            ...
            <pre-post> <form>)

  where <pre-post> is anything that contains keys `:pre` and `:post`.
  If a key is not supplied then the test is passed. This allows to only
  run pre/post condtions or a mix of both. An empty map means no tests are run.

  When expr conforms to the supplied spec, threads it into the first form (via ->>),
  and when that result conforms, through the next etc"
  [expr & forms]
  (let [g (gensym)
        steps (map (fn [[spec step]] `(clojure.core/if-let [pre# (:pre ~spec)]
                                        (if-not (s/valid? pre# ~g)
                                          (throw-spec ~spec ~g)
                                          (let [res# (->> ~g ~step)]
                                            (clojure.core/if-let [post# (:post ~spec)]
                                              (if-not (s/valid? post# res#)
                                                (throw-spec ~spec res#)
                                                res#)
                                              res#)))
                                        (->> ~g ~step)))
                   (partition 2 forms))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(defmacro as->
  "Similar to `as->` but `forms` is a sequence of clauses.

   (as-> <expr> <name>
         <spec> <form>
         <spec> <form>
         ...
         <spec> <form>)

   Binds name to expr, evaluates the first form in the lexical context
   of that binding, then binds name to that result, repeating for each
   successive form, returning the result of the last form.

   Important - Variable scoping, the `name` is currently exposed to specs
   should be treated with caution

   (as-> 100 $
         (s/and number? even?) (+ $ 50)
         (fn [x] (= x $)) :name-was-exposed-to-spec
         keyword? (identity $))
  "
  [expr name & forms]
  (let [steps (map (fn [[spec step]] `(if (try (s/valid? ~spec ~name)
                                               (catch Exception e#
                                                 (throw-spec ~spec ~name)))
                                        ~step
                                        (throw-spec ~spec ~name)))
                   (partition 2 forms))]
    `(only-catch-spec-errors (let [~name ~expr
                                   ~@(interleave (repeat name) (butlast steps))]
                               ~(if (empty? steps)
                                  name
                                  (last steps))))))

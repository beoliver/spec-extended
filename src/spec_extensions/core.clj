(ns spec-extensions.core
  (:require [clojure.spec :as s]
            [spec-extensions.errors :refer [throw-spec catch-errors-valid?]]))

(declare spec-if-let)
(declare spec-if-lets)
(declare spec-when-let)
(declare spec-when-let!)
(declare spec-when-lets)
(declare spec-when-lets!)
(declare spec-some->)
(declare spec-some->!)
(declare spec-some->>)
(declare spec-some->>!)
(declare spec->)
(declare spec->!)
(declare spec->>)
(declare spec->>!)

(defmacro spec-if-let
  "an extension of `if-let`. Only executes the `then` when the bound value
   conforms to `spec`.

   (spec-if-let <spec> <bindings> <then>)
   (spec-if-let <spec> <bindings> <then> <else>)

   like if-let, <bindings> is of the form [<var> <expr>]

   examples
   (spec-if-let even? [x 10] (println x))
   (spec-if-let ::my-spec [x (some-fn)] (some-other-fn x) :else-branch)
   (spec-if-let nil? [x (println 1)] :printed :didnt-print)
  "
  ([spec bindings then]
   `(spec-if-let ~spec ~bindings ~then nil))
  ([spec bindings then else]
   (let [form (bindings 0) rhs (bindings 1)]
     `(let [res# ~rhs]
        (if (s/valid? ~spec res#)
          (let [~form res#]
            ~then)
          ~else)))))

(defmacro spec-when-let
  "an extension of `when-let`. Only executes the `then` when the bound value
   conforms to `spec`.

  (spec-when-let <spec> <bindings> <then>)
  (spec-when-let <spec> <bindings> <then> <else>)

  like when-let, <bindings> is of the form [<var> <expr>]

  example
  (spec-when-let even? [x 10] (println x))
  (spec-if-let ::my-spec [x (some-fn)] (some-other-fn x))
  (spec-when-let nil? [x (println 1)] :printed)
  "
  [spec bindings then]
  (let [form (bindings 0) rhs (bindings 1)]
    `(let [res# ~rhs]
       (when (s/valid? ~spec res#)
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

(defmacro spec-if-lets
  "allows multiple bindings, where bindings take the form
  [[<var-1> <expr-1>] <spec-1> ... [<var-n> <expr-n>] <spec-n>]

  example
  (spec-if-lets [[x 1] odd? [y 2] ::my-spec] (+ x y) :boop)
  "
  ([bindings then]
   `(spec-if-lets ~bindings ~then nil))
  ([bindings then else]
   (if (seq bindings)
     `(spec-if-let* [~(first bindings) ~(second bindings)]
        (spec-if-lets ~(drop 2 bindings) ~then ~else)
        ~else)
     then)))

(defmacro spec-when-lets
  "allows multiple bindings, where bindings take the form
  [[<var-1> <expr-1>] <spec-1> ... [<var-n> <expr-n>] <spec-n>]

  example
  (spec-when-lets [[x 1] odd? [y 2] ::my-spec] (+ x y))
  "
  ([bindings then]
   (if (seq bindings)
     `(spec-if-let* [~(first bindings) ~(second bindings)]
        (spec-when-lets ~(drop 2 bindings) ~then)
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

;; (defmacro some->
;;   "When expr is not nil, threads it into the first form (via ->),
;;   and when that result is not nil, through the next etc"
;;   {:added "1.5"}
;;   [expr & forms]
;;   (let [g (gensym)
;;         steps (map (fn [step] `(if (nil? ~g) nil (-> ~g ~step)))
;;                    forms)]
;;     `(let [~g ~expr
;;            ~@(interleave (repeat g) (butlast steps))]
;;        ~(if (empty? steps)
;;           g
;;           (last steps)))))

(defmacro spec-some->
  "Similar to `some->` but `forms` is a sequence of clauses.

  (spec-some-> <expr>
               <spec-1> <form-1>
               <spec-2> <form-2>
               ...
               <spec-n> <form-n>)

  Each <spec> is treated as a pre-condition.
  When expr conforms to the supplied spec, threads it into the first form (via ->),
  and when that result conforms, through the next etc"
  [expr & forms]
  (let [g (gensym)
        steps (map (fn [[spec step]] `(if (s/valid? ~spec ~g) (-> ~g ~step) nil))
                   (partition 2 forms))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(defmacro spec-some->
  "Similar to `some->` but `forms` is a sequence of clauses.

  (spec-some-> <expr>
               <spec-1> <form-1>
               <spec-2> <form-2>
               ...
               <spec-n> <form-n>)

  Each <spec> is treated as a pre-condition.
  When expr conforms to the supplied spec, threads it into the first form (via ->),
  and when that result conforms, through the next etc.

  As soon as a pre-condition fails, the execution is aborted.
  This avoids the possiblity that a `nil` is propogated though the chain.
  "
  [expr & forms]
  (let [g (gensym)
        steps (map (fn [[spec step]] `(if (s/valid? ~spec ~g)
                                        (-> ~g ~step)
                                        (throw (Exception.))))
                   (partition 2 forms))]
    `(try (let [~g ~expr
                ~@(interleave (repeat g) (butlast steps))]
            ~(if (empty? steps)
               g
               (last steps)))
          (catch Exception e# nil))))

(defmacro spec-some->>
  "Similar to `some->>` but `forms` is a sequence of clauses.

   (spec-some->> <expr>
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
                                        (throw (Exception.))))
                   (partition 2 forms))]
    `(try (let [~g ~expr
                ~@(interleave (repeat g) (butlast steps))]
            ~(if (empty? steps)
               g
               (last steps)))
          (catch Exception e# nil))))

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
        steps (map (fn [[spec step]] `(if-let [pre# (:pre ~spec)]
                                        (when (s/valid? pre# ~g)
                                          (let [res# (-> ~g ~step)]
                                            (if-let [post# (:post ~spec)]
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
        steps (map (fn [[spec step]] `(if-let [pre# (:pre ~spec)]
                                        (if-not (s/valid? pre# ~g)
                                          (throw-spec ~spec ~g)
                                          (let [res# (-> ~g ~step)]
                                            (if-let [post# (:post ~spec)]
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
        steps (map (fn [[spec step]] `(if-let [pre# (:pre ~spec)]
                                        (when (s/valid? pre# ~g)
                                          (let [res# (->> ~g ~step)]
                                            (if-let [post# (:post ~spec)]
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
        steps (map (fn [[spec step]] `(if-let [pre# (:pre ~spec)]
                                        (if-not (s/valid? pre# ~g)
                                          (throw-spec ~spec ~g)
                                          (let [res# (->> ~g ~step)]
                                            (if-let [post# (:post ~spec)]
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



;; (defmacro as*->
;;   "Binds name to expr, evaluates the first form in the lexical context
;;   of that binding, then binds name to that result, repeating for each
;;   successive form, returning the result of the last form."
;;   {:added "1.5"}
;;   [expr name & forms]
;;   `(let [~name ~expr
;;          ~@(interleave (repeat name) (butlast forms))]
;;      ~(if (empty? forms)
;;         name
;;         (last forms))))


;; (defmacro spec-as->
;;   [expr name & forms]
;;   `(let [~name ~expr
;;          [form# condtions#] ~(first forms)]
;;      form#
;;      )

;;   )

;; (defmacro spec-as->
;;   [expr name & forms]
;;   `(let [~name ~expr
;;          constraints# ~(second (first forms))]
;;      (if-not (:pre constraints#)
;;        (let [result# ~(first (first forms))]
;;          )
;;        (when (s/valid? (:pre constraints#) ~name)
;;          (let [result# ~(first (first forms))]
;;            (if (:post constraints#)
;;              (when (s/valid? (:post constraints#) result#)
;;                ~(spec-as-> name result#)
;;                ))))



;;        )
;;      conditions#
;;      )

;;   )

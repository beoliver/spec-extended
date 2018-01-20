# spec-extended

A library that provides spec extended versions of clojure's conditional and threading macros.
In all of the following examples `clojure.spec` has been impoted as `s`, while `spec-extended.core` has been imported as `se`.

## Example

```clojure
(s/def ::gt-than-fifty #(> % 50))

(se/when-let (s/and even? ::gt-than-fifty) [x (rand-int 100)]
  (println "valid value was" x))
```

## Properties

Lets assume that we have defined some spec, say:
```clojure
(s/def ::my-even even?)
```
The call `(s/valid? ::my-even 0)` will return `true` while `(s/valid? ::my-even 1)` will return `false`.
If however we try the following `(s/valid? ::my-even nil)` we are greeted with the following:
```clojure
IllegalArgumentException Argument must be an integer:   clojure.core/even? (core.clj:1383)
```

While clojure does not expose a type system in the same way as ML (or Java etc) it is usefull to think abstractly for
a minute. Instead of talking about types, we will talk about sets.

- A function `f : X -> Y` is said to be **total** if for every element x in X then f(x) in Y.
- A function `f : X -> Y` is said to be **partial** there is an element x in X then f(x) is not in Y.
Note that **partial** in this context is different from `(partial f & args)` which refers to **partial application**

Let's assume that we have a set `T` that contains **every single valid clojure value**
```clojure
1, {:hello "world"}, ["a" :foo (fn [x] x) java.lang.Float], ...
```

Now, when writing a *spec* `p` that is **total** with respect to `T` it must have the from
```haskell
p : T -> {true, false}
```

### But what about Exceptions?
Should testing to see if a value conforms to a spec ever cause an exception to be thrown? Perhaps this is a philosophical question, but
never the less it should be considered.

If we assume that our specs are **pure**

 | pure  | total | logical errors | throw exception |
 | true  | true  | false          | false           |
 | true  | true  | true           | true            |
 | false | true  | false          | true            |
 | false | true  | false          | true


This begs the question... Should we provide a *stricter* spec? i.e should we ensure that the spec is **total** - Perhaps something like:
```clojure
(s/def ::my-stricter-even (s/and number? even?)) ; note the importance of order
```

Ideally we want some kind of function that ensures that our predicates are total
`(forall S \subseteq C*) make-predicate-total : (S -> Boolean) -> (C* -> Boolean)
If we were only concerned with predicates then this would be simple, but we need to deal with `specs`. The simplest solution is to write a new `valid?` function
such that if `spec : (S -> Boolean)` then `(parital total-valid? spec) : (C* -> Boolean)`
```clojure
(defn total-valid?
  [spec expr]
  (try (s/valid? spec expr)
       (catch Exception _ false)))
```
Note however this this approach does have its drawbacks. Assume that we have a spec that can throw an exception, in this case for all even numbers it will perform a division by zero.
```clojure
(s/def ::silly-spec
  (s/and even?
         (fn [x] (/ x 0))))
```
With the `total-valid?` function we will just return `false`. Perhaps we could examine the stack trace and only hide `IllegalArgument Exception`, but it is hard to be consistent with this approach.

All approaches have their benefits and their drawbacks. If our specs are **pure** then catching exceptions is of no real consequence, an error thrown during validation is most likely type based, structural or due to some logical error in the spec itself (division by 0). However, if our spec uses some form of state, it would probably be wise to expose the error. Indeed, in some cirsumstances it would be very nice to know when our system is and is not acting as expected.

For this reason, `spec-extended` aims to provide three versions of each macro form.
- A version that hides validation exceptions. An exception thrown during validation is treated in the same way as `(not (s/valid? <expr>))`.
- A version with a single `!` suffix that exposes validation exceptions.
- A version with a double `!!` suffix that exposes validation exceptions and throws an exception when some entity does not conform to a spec.

### scope of variables

As spec-extended macros introduce clauses, users should be aware of scoping implications. Most (all?) of clojures threading macros exapand into let statements. Clojure makes variables bound at position n in a let expresions available to subsequent expressions. This also means that bound variables are accessable by spec definitions.
- Need to check, but if a spec refers to some **global** variable, and the let bindings introduce a variable that shadows that global, then it will be used in place. Will work on this, think about what is the best approach.

### `if-let` and `when-let`
The most trivial and possibly most useful macro is the spec extended `if-let` form.

```clojure
(s/def ::gt-than-fifty #(> % 50))

(se/if-let (s/and even? ::gt-than-fifty) [x (rand-int 100)]
  (println "valid value was" x)
  (println "the else branch"))
```
In the above example `(rand-int 100)` is computed, and if the resulting value is `spec/valid?` with respect to the composed spec `(s/and even? ::gt-than-fifty)` the *then* branch is executed. In a similar fashion there is a `when-let`.
```clojure
(se/when-let (s/and even? ::gt-than-fifty) [x (rand-int 100)]
  (println "valid value was" x))
```
As mentioned in the Properties section, there are `if-let!` and `if-let!!` variants

```clojure
;; this will throw an error due to (even? nil)
(se/if-let! even? [x nil]
  (println "valid value was" x)
  (println "the else branch"))

;; this will throw an error due to (even? 1) being false
(se/if-let!! even? [x 1]
  (println "valid value was" x)
  (println "the else branch"))
```

### `some->` and `some->>`
The standard `some->` and `some->>` threading macros uses *nil punning* - it is assumed that if any form returns `nil` then no more forms should be evaluated.

The **spec extended** version of these macros takes the form:

```clojure
(some-> <expr>
        <spec> <form>
        <spec> <form>
        ...
        <spec> <form>)
```
Each `<spec>` is treated as a pre condition for each `<form>`. As soon as a `<spec>` is not `s/valid?` the value `nil` is returned (even if the next spec is `nil?`). One important aspect is that as testing a spec can result in an error being thrown, `spec-extended` treats any exceptions thrown in the validation process as **invalid**. If an exception is thrown while evaluating a `<form>` then the error will be thrown.

```clojure
(se/some-> 0
           even? inc
           odd?  inc)
```

### `as->`

```clojure
(ns my-project
  (:require [clojure.spec :as s]
            [spec-extended.core :as se]))

(s/def ::my-even even?)

(se/as-> 100 $
         (s/and number? ::my-even) (+ $ 50)
	 even? (do (println "hello from spec-extentions") $)
	 odd? (println "this will never execute"))
```

## Usage

FIXME

## License

Copyright © 2018 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

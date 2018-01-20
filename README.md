# spec-extended

A library that provides spec extended versions of clojure's conditional and threading macros.
In all of the following examples `clojure.spec` has been impoted as `s`, while `spec-extended.core` has been imported as `se`.

## Example

```clojure
(s/def ::gt-than-fifty #(> % 50))

(se/when-let (s/and even? ::gt-than-fifty) [x (rand-int 100)]
  (println "valid value was" x))
```

## Rationale

What happens when you take conditional threading, for example `some->` and extend it to handle `clojure.spec` definitions?
Spec based composable workflows. Instead of having to over specify the input and output parameters for each function,
**spec-extended** allows you to compose existing functions but with additional guarantees that a spec might provide.

As clojure does not really *do* types, instead of tring to treat it as a typed language we can think of it as something else. What that is, and what spec will lead to is not clear. When not restricted to types, functions and procedures are often *context sensitive*. While this is also true for typed languages clojure allows us to take that to extremes. Having the ability to create workflows that enforce certain invariants is one approach.

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

This begs the question... Should it be the job of the deveolper to provide a *stricter* spec? i.e ensure that the spec is **total** - Perhaps something like:
```clojure
(s/def ::my-stricter-even (s/and number? even?)) ; note the importance of order
```
Or should the library provide some extended (perhaps implicit) functionality? For example:
```clojure
(defn total-valid?
  [spec expr]
  (try (s/valid? spec expr)
       (catch Exception _ false)))
```
This function acheives a kind of totality - return values are guaranteed to be `true` or `false` for any input element, however
how do we handle the case of a spec that throws an exception not because of a type error, but because of some faulty logic?
```clojure
(s/def ::silly-spec
  (s/and even?
         (fn [x] (= x (/ x 0))))
```
In this example for all even numbers it will perform a division by zero resulting in an exception. As far as a user of `total-valid?` is aware,
no value will ever satisfy this spec. Would the exception have made debugging easier? probably! Perhaps we could examine the stack trace and only
hide `IllegalArgument Exception`, but it is hard to be consistent with this approach. The take away here is that while a library can provide
mechanisms for making partial functions total, some information may be lost in the process.

### So... how do you handle Exceptions?
Should testing to see if a value conforms to a spec ever cause an exception to be thrown? Perhaps this is a philosophical question, but
never the less it should be considered.

 | pure  | total | logical errors | may throw exception |
 | :---: | :---: | :---:          | :---:           |
 | true  | true  | false          | false           |
 | ...   | ...   | ...            | true            |

The logical errors table captures for example *division by zero* and other related issues.

For this reason, `spec-extended` aims to provide three versions of each macro form.
- A version that hides validation exceptions. An exception thrown during validation is treated in the same way as `(s/valid? <expr>)` returning `false`.
- A version with a single `!` suffix that exposes validation exceptions.
- A version with a double `!!` suffix that exposes validation exceptions and throws an exception when some entity does not conform to a spec.

You can think of each `!` as introducing a level of *strictness*.

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

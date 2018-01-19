# spec-extensions

A library that extends `clojure.spec`

Aims to provide spec extended versions of clojure's conditional and threading macros.

In all of the following examples `clojure.spec` has been impoted as `s`, while `spec-extended.core` has been imported as `se`.

## Properties

Lets assume that we have defined some spec, say:
```clojure
(s/def ::my-even even?)
```
The call `(s/valid? ::my-even 0)` will return `true` while `(s/valid? ::my-even 1)` will return `false`. If however we try the following `(s/valid? ::my-even nil)` we are greeted with the following:
```clojure
IllegalArgumentException Argument must be an integer:   clojure.core/even? (core.clj:1383)
```
This begs the question, should we have provided a *stricter* `spec`? Perhaps something like this:
```clojure
(s/def ::my-stricter-even (s/and number? even?)) ;;note the importance of order
```
Or could we *relax* the notion of validity (i.e don't throw an exception)
```clojure
(defmacro catch-errors-valid?
  [spec expr]
  `(try (s/valid? ~spec ~expr)
        (catch Exception e# nil)))
```
Both approaches have their benefits and their drawbacks. By default `spec-extended` catches exceptions thrown during validation and treats this as an sign that the value passed to it did not conform. However, in some cirsumstances it would be very nice to know when our system is not acting as expected. For this reason, a **bang** version of each macro is provided which will throw an error if a value is not `s/valid?`. If the validation process does cause an exception it will be placed in an `ex-info` map logging its context.



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
            [spec-extensions.core :as se]))

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

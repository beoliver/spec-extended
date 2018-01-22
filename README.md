# spec-extended

A library that provides spec extended versions of clojure's conditional and threading macros.
In all of the following examples `clojure.spec` has been impoted as `s`, while `spec-extended.core` has been imported as `se`.

## Example

```clojure
=> (s/def ::gt-than-fifty #(> % 50))

=> (se/conforms-> (rand-int 100) even?
                  inc            ::gt-than-fifty
                  println        any?)
71
nil

=> (se/conforms-> (rand-int 100) even?
                  inc            ::gt-than-fifty
                  println        any?)
:clojure.spec/invalid
```

## Rationale

What happens when you take conditional threading, for example `some->` and extend it to handle `clojure.spec` definitions?
Spec based composable workflows. Instead of having to over specify the input and output parameters for each function,
**spec-extended** allows you to compose existing functions but with additional guarantees that a spec might provide.

As clojure does not really *do* types, instead of tring to treat it as a typed language we can think of it as something else. What that is, and what spec will lead to is not clear. Functions and procedures are often *context sensitive* and while this is also true for typed languages clojure allows us to take that to extremes. Having the ability to create workflows and compose those workflows, while enforcing certain invariants is just one way of exploring the space.

## Properties

`clojure.spec` uses a namespaced keyword `:clojure.spec/invalid` to represent when a value does not `clojure.spec/conform` to a given spec. While this has introduced some [issues](https://dev.clojure.org/jira/browse/CLJ-1966), this library treats `:clojure.spec/invalid` in a **special** way.

```clojure
> (def f #(some-> % inc inc))
> (def g #(some-> % dec dec))
> (def h (comp g f))

> (h 1)
1
> (h 2)
2
> (h nil)
nil
> (h "hello")
ClassCastException java.lang.String cannot be cast to java.lang.Number  clojure.lang.Numbers.dec (Numbers.java:120)
```
### Composition
If `f` and `g` are forms construced using spec-extended then `(comp g f)` is a function using spec-extended.
```clojure
> (def f #(conforms-> % number? inc any? inc even?))
> (def g #(conforms-> % (s/and number? even?) dec any? dec any?))
> (def h (comp g f))

> (h 1)
:clojure.spec/invalid
> (h 2)
2
> (h nil)
:clojure.spec/invalid
> (h "hello")
:clojure.spec/invalid
```



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

- A function `f : X -> Y` is said to be **total** if for every element x in X, an element f(x) is in Y.
- A function `f : X -> Y` is said to be **partial** if there is an element x in X where f(x) is not in Y.
Note that **partial** in this context is different from `(partial f & args)` which refers to **partial application**.

Let's assume that we have an (infinite) set `T` that contains **every single valid clojure value**
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
`spec-extended` does not attempt to hide any validation errors.

### scope of variables

As spec-extended macros introduce clauses, users should be aware of scoping implications. Most (all?) of clojures threading macros exapand into let statements. Clojure makes variables bound at position n in a let expresions available to subsequent expressions. This also means that bound variables are accessable by spec definitions.
- Need to check, but if a spec refers to some **global** variable, and the let bindings introduce a variable that shadows that global, then it will be used in place. Will work on this, think about what is the best approach.

### `conforms->` and `conforms->>`

While the standard `some->` and `some->>` threading macros use *nil punning* - i.e it is assumed that if any form returns `nil` then the resulting `nil` should not be passed to any more forms. `conforms->` and `conforms->>` takes a different approach. Each value is tested to see if `(spec/conform x)` returns `:clojure.spec/invalid`. If this is the case then `:clojure.spec/invalid` will be returned.

To get a clearer idea we can macroexpand the form to see what it looks like under the hood.

```clojure
=> (macroexpand '(conforms-> (rand-int 100) even?
                             inc            ::gt-than-fifty
                             println        any?))

(let* [G__17316 (clojure.spec/conform even? (rand-int 100))
       G__17316 (if (clojure.spec/invalid? G__17316)
                  G__17316
                  (clojure.spec/conform :spec-extended.core/gt-than-fifty
                                        (clojure.core/-> G__17316 inc)))]
  (if (clojure.spec/invalid? G__17316) G__17316
      (clojure.spec/conform any? (clojure.core/-> G__17316 println))))
```

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

### `as->`

```clojure
(s/def ::my-even even?)

(as-> 100 $ (s/and number? ::my-even)
      (+ $ 50) even?
      (do (println "hello from spec-extentions") $) even?
      (cons $ nil) list?)
```

## Usage

FIXME

## License

Copyright © 2018 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

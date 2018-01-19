# spec-extensions

A library that extends `clojure.spec`

Aims to provide spec extended versions of clojures branching and threading macros.

The most trivial and possibly most useful macro is the spec extended `if-let` form.

```clojure
(ns my-project
  (:require [clojure.spec :as s]
            [spec-extensions.core :as se]))

(s/def ::even even?)

(se/if-let ::even [x (rand-int 100)]
  (println "even value was" x)
  (println "number must have been odd"))
```

In the above example `::even` is treated as a post condition. `(rand-int 100)` is computed, and if the resulting value is `spec/valid?` with respect to `::even` the *then* branch is executed.


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

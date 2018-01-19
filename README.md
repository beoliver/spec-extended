# spec-extensions

A library that extends `clojure.spec`

Aims to provide spec extended versions of clojures branching and threading macros.

The most trivial and possibly most useful macro is the spec extended `if-let` form.

### `if-let`

```clojure
(ns my-project
  (:require [clojure.spec :as s]
            [spec-extensions.core :as se]))

(s/def ::gt-than-fifty #(> % 50))

(se/if-let (s/and even? ::gt-than-fifty) [x (rand-int 100)]
  (println "valid value was" x)
  (println "the else branch"))
```

In the above example `(rand-int 100)` is computed, and if the resulting value is `spec/valid?` with respect to the composed spec `(s/and even? ::gt-than-fifty)` the *then* branch is executed. In a similar fashion there is a `when-let`.




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

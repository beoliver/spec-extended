(ns spec-extended.core-test
  (:require [clojure.test :refer :all]
            [clojure.spec :as s]
            [spec-extended.core :as se]))

(defmacro try-spec-expr
  [expr]
  `(try ~expr
        (catch Exception e# (:type (ex-data e#)))))

(s/def ::even even?)

(s/def ::even-string?
  (s/and string?
         #(even? (count %))))

(deftest if-let-test
  (is (= :ok
         (se/if-let even? [x 2] :ok :err)))
  (is (= :err
         (se/if-let even? [x nil] :ok :err)))
  (is (= :ok
         (se/if-let ::even [x 2] :ok :err)))
  (is (= :err
         (se/if-let odd? [x 2] :ok :err)))
  (is (= :ok
         (se/if-let ::even-string? [x "abcd"] :ok :err)))
  (is (= :err
         (se/if-let ::even-string? [x "abc"] :ok :err)))
  (is (= :err
         (se/if-let ::even-string? [x 2] :ok :err))))

(deftest if-lets-test
  (is (= 5
         (se/if-lets [[x 2] even?
                      [y 3] odd?] (+ x y))))
  (is (= :err
         (se/if-lets [[x 2] odd?
                      [y 3] odd?] (+ x y) :err)))
  (is (= "0ab"
         (se/if-lets [[x 0] even?
                      [y "ab"] ::even-string?] (str x y))))
  (is (= "0ab"
         (se/if-lets [[x 0] even?
                      [y "ab"] (s/and string? #(even? (count %)))]
                     (str x y))))
  (is (= :err
         (se/if-lets [[x 0] even?
                      [y "a"] ::even-string?] (str x y) :err)))
  (is (nil?
       (se/if-lets [[x 1] even?
                    [y "ab"] ::even-string?] (str x y)))))

(deftest when-let-test
  (is (= :ok
         (se/when-let even? [x 2] :ok)))
  (is (= :ok
         (se/when-let ::even [x 2] :ok)))
  (is (= nil
         (se/when-let odd? [x 2] :ok)))
  (is (= :ok
         (se/when-let ::even-string? [x "abcd"] :ok)))
  (is (= nil
         (se/when-let ::even-string? [x "abc"] :ok)))
  (is (= nil
         (se/when-let ::even-string? [x 2] :ok))))

(deftest when-lets-test
  (is (= 5
         (se/when-lets [[x 2] even?
                        [y 3] odd?] (+ x y))))
  (is (= "0ab"
         (se/when-lets [[x 0] even?
                        [y "ab"] ::even-string?] (str x y))))
  (is (nil?
       (se/when-lets [[x 1] even?
                      [y "ab"] ::even-string?] (str x y)))))

(deftest spec-when-let!-test
  (is (= :ok
         (try-spec-expr
          (se/spec-when-let! even? [x 2] :ok))))
  (is (= :ok
         (se/spec-when-let! ::even [x 2] :ok)))
  (is (= :spec-extensions.errors/invalid
         (try-spec-expr
          (se/spec-when-let! odd? [x 2] :ok))))
  (is (= :ok
         (se/spec-when-let! ::even-string? [x "abcd"] :ok)))
  (is (= :spec-extensions.errors/invalid
         (try-spec-expr
          (se/spec-when-let! ::even-string? [x "abc"] :ok))))
  (is (= :spec-extensions.errors/invalid
         (try-spec-expr (se/spec-when-let! ::even-string? [x 2] :ok)))))


(deftest some->test
  (is (= 2 (se/some-> 0
                      even? inc
                      odd?  inc)))
  (is (nil? (se/some-> 0
                       even? inc
                       even?  inc)))
  (is (nil? (se/some-> 0
                       odd? inc
                       odd? inc)))
  (testing "any? does not check"
    (is (nil? (se/some-> nil
                         number? inc)))
    (is (= :err (try (se/some-> nil
                                any? inc)
                     (catch Exception e :err))))))



;; (macroexpand '(spec-some-> 0 even? inc even? inc even? inc))

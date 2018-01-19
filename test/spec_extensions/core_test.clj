(ns spec-extensions.core-test
  (:require [clojure.test :refer :all]
            [clojure.spec :as s]
            [spec-extensions.core :refer :all]))

(defmacro try-spec-expr
  [expr]
  `(try ~expr
        (catch Exception e# (:type (ex-data e#)))))

(s/def ::even even?)

(s/def ::even-string?
  (s/and string?
         #(even? (count %))))

(deftest spec-if-let-test
  (is (= :ok
         (spec-if-let even? [x 2] :ok :err)))
  (is (= :ok
         (spec-if-let ::even [x 2] :ok :err)))
  (is (= :err
         (spec-if-let odd? [x 2] :ok :err)))
  (is (= :ok
         (spec-if-let ::even-string? [x "abcd"] :ok :err)))
  (is (= :err
         (spec-if-let ::even-string? [x "abc"] :ok :err)))
  (is (= :err
         (spec-if-let ::even-string? [x 2] :ok :err))))

(deftest spec-if-lets-test
  (is (= 5
         (spec-if-lets [[x 2] even?
                        [y 3] odd?] (+ x y))))
  (is (= :err
         (spec-if-lets [[x 2] odd?
                        [y 3] odd?] (+ x y) :err)))
  (is (= "0ab"
         (spec-if-lets [[x 0] even?
                        [y "ab"] ::even-string?] (str x y))))
  (is (= "0ab"
         (spec-if-lets [[x 0] even?
                        [y "ab"] (s/and string? #(even? (count %)))]
                       (str x y))))
  (is (= :err
         (spec-if-lets [[x 0] even?
                        [y "a"] ::even-string?] (str x y) :err)))
  (is (nil?
       (spec-if-lets [[x 1] even?
                      [y "ab"] ::even-string?] (str x y)))))

(deftest spec-when-let-test
  (is (= :ok
         (spec-when-let even? [x 2] :ok)))
  (is (= :ok
         (spec-when-let ::even [x 2] :ok)))
  (is (= nil
         (spec-when-let odd? [x 2] :ok)))
  (is (= :ok
         (spec-when-let ::even-string? [x "abcd"] :ok)))
  (is (= nil
         (spec-when-let ::even-string? [x "abc"] :ok)))
  (is (= nil
         (spec-when-let ::even-string? [x 2] :ok))))

(deftest spec-when-lets-test
  (is (= 5
         (spec-when-lets [[x 2] even?
                          [y 3] odd?] (+ x y))))
  (is (= "0ab"
         (spec-when-lets [[x 0] even?
                          [y "ab"] ::even-string?] (str x y))))
  (is (nil?
       (spec-when-lets [[x 1] even?
                        [y "ab"] ::even-string?] (str x y)))))

(deftest spec-when-let!-test
  (is (= :ok
         (try-spec-expr
          (spec-when-let! even? [x 2] :ok))))
  (is (= :ok
         (spec-when-let! ::even [x 2] :ok)))
  (is (= :spec-extensions.errors/invalid
         (try-spec-expr
          (spec-when-let! odd? [x 2] :ok))))
  (is (= :ok
         (spec-when-let! ::even-string? [x "abcd"] :ok)))
  (is (= :spec-extensions.errors/invalid
         (try-spec-expr
          (spec-when-let! ::even-string? [x "abc"] :ok))))
  (is (= :spec-extensions.errors/invalid
         (try-spec-expr (spec-when-let! ::even-string? [x 2] :ok)))))


(deftest spec-some->-test
  (is (= 2 (spec-some-> 0
                        even? inc
                        odd?  inc)))
  (is (nil? (spec-some-> 0
                         even? inc
                         even?  inc)))
  (is (nil? (spec-some-> 0
                         odd? inc
                         odd? inc))))



;; (macroexpand '(spec-some-> 0 even? inc even? inc even? inc))

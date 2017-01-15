(ns kibe.core-test
  (:require [clojure.test :refer :all]
            [kibe.core :refer :all]))

(deftest failure-test
  (let [target (failure true)]
    (is (satisfies? IFailure target))
    (is (true? (fail-unwrap target)))))

(deftest success-test
  (let [target (success true)]
    (is (satisfies? ISuccess target))
    (is (true? (success-unwrap target)))))

(deftest success?-test
  (is (success? (success true))))

(deftest failure?-test
  (is (failure? (failure true))))

(deftest handle-test
  (testing "failure"
    (handle
     [target (failure true)]
     (throw (Exception. "Error, should've gone down the other path"))
     (is (true? target))))
  (testing "success"
    (handle
     [target (success true)]
     (is (true? target))
     (throw (Exception. "Error, should've gone down the other path")))))

(defh test-1
  "A sample handler"
  [a b]
  (if (> a b)
    (success (- a b))
    (failure {:error true})))

(defh test-2
  "A sample handler"
  [a b]
  (success (/ a b)))

(deftest defh-test
  (testing "test-1"
    (handle
     [result (call test-1 [2 1])]
     (is (= 1 result))
     (throw (Exception. "Error, should've gone down the other path")))
    (handle
     [result (call test-1 [1 1])]
     (throw (Exception. "Error, should've gone down the other path"))
     (is (= {:error true} result))))
  (testing "test-2"
    (handle
     [result (call test-2 [2 1])]
     (is (= 2 result))
     (throw (Exception. "Error, should've gone down the other path")))
    (handle
     [result (call test-2 [1 0])]
     (throw (Exception. "Error, should've gone down the other path"))
     (is (:exception result)))))

(defh test-3
  "A sample handler"
  [a b c]
  (success (+ (* a a) (- b c))))

(defh test-4
  "A sample handler"
  [a b c d]
  (cond?>
   a
   (test-1 b)
   (test-3 c d)
   (test-1 b)))

(defh test-5
  "A sample handler"
  [a b c d]
  (cond?>>
   a
   (test-1 b)
   (test-3 c d)
   (test-1 b)))

(deftest cond?>-test
  (handle
   [result (test-4 25 5 2 3)]
   (is (= 394 result))
   (throw (Exception. "Error, should've gone down the other path")))

  (handle
   [result (test-4 25 5 2 10)]
   (is (= 387 result))
   (throw (Exception. "Error, should've gone down the other path")))

  (handle
   [result (test-4 0 5 2 10)]
   (throw (Exception. "Error, should've gone down the other path"))
   (is (= {:error true} result)))

  (handle
   [result (test-4 6 5 0 0)]
   (throw (Exception. "Error, should've gone down the other path"))
   (is (= {:error true} result))))

(deftest cond?>>-test
  (handle
   [result (test-5 5 25 2 3)]
   (is (= 38 result))
   (throw (Exception. "Error, should've gone down the other path")))

  (handle
   [result (test-5 5 25 2 10)]
   (is (= 31 result))
   (throw (Exception. "Error, should've gone down the other path")))

  (handle
   [result (test-5 5 0 2 10)]
   (throw (Exception. "Error, should've gone down the other path"))
   (is (= {:error true} result)))

  (handle
   [result (test-5 5 6 0 7)]
   (throw (Exception. "Error, should've gone down the other path"))
   (is (= {:error true} result))))

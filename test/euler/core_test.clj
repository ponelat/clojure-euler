(ns euler.core-test
  (:require [clojure.test :refer :all]
            [euler.core :refer :all]))

(deftest eulers-tests
  (testing "Euler problem #1"
    (is (= (euler-1) 233168)))
  (testing "Euler problem #2"
    (is (= (euler-2) 4613732)))
  (testing "Euler problem #3"
    (is (= (euler-3) 6857)))
  (testing "Euler problem #4"
    (is (= (euler-4) 906609)))
  (testing "Euler problem #5"
    (is (= (euler-5) 232792560))))

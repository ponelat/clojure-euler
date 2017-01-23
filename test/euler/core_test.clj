(ns euler.core-test
  (:require [clojure.test :refer :all]
            [euler.core :refer :all]))

(deftest a-test
  (testing "Euler problem #1"
    (is (= (euler-1) 233168))))

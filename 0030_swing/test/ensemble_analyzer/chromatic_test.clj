(ns ensemble-analyzer.chromatic-test
  (:require [clojure.test :refer :all]
            [ensemble-analyzer.chromatic :refer :all]))


(deftest a-test
  (testing "testcase 1"
    (prn (index 0.25 256.0 10 4.0 16))
    (is true)))

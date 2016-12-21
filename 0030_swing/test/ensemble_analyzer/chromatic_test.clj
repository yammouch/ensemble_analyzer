(ns ensemble-analyzer.chromatic-test
  (:require [clojure.test :refer :all]
            [ensemble-analyzer.chromatic :refer :all]))


(deftest a-test
  (testing "testcase 1"
    (prn (lin-to-log 32.0 1.0 5
                     [ 1.0 ;  0 Hz ( 0- 2 Hz)
                       2.0 ;  4 Hz ( 2- 6 Hz)
                       3.0 ;  8 Hz ( 6-10 Hz)
                       4.0 ; 12 Hz (10-14 Hz)
                       5.0 ; 16 Hz (14-18 Hz)
                       6.0 ; 20 Hz (18-22 Hz)
                       7.0 ; 24 Hz (22-26 Hz)
                       8.0 ; 28 Hz (26-30 Hz)
                       9.0 ; 32 Hz (30-34 Hz)
                      10.0 ; 36 Hz (34-38 Hz)
                      11.0 ; 40 Hz (38-42 Hz)
                      ]
                     4.0))
    (is true)))

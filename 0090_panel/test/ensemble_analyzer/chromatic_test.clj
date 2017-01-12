(ns ensemble-analyzer.chromatic-test
  (:require [clojure.test :refer :all]
            [ensemble-analyzer.chromatic :refer :all]))

(deftest a-test
  (is (= (index 0.25 256.0 10 4.0 16)
         [[0] [0] [0] [1] [1] [2 3] [4 5 6 7]
          [8 9 10 11 12 13 14 15] [] []
          ]))
  (is (= (pickup [[0] [0] [1 2] [3 4 5] [] []]
                 (vec (map double [0 1 2 3 4 5])))
         [0.0 0.0 1.5 4.0 0.0 0.0])))

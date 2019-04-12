(ns hash-test
  (:require [clojure.test :refer :all]
            [hash :refer :all]))

(def ^:private sha256-digest-hi "8f434346648f6b96df89dda901c5176b10a6d83961dd3c1ac88b59b2dc327aa4")

(deftest basic-hash
  (testing "can get the correct digest"
    (is (= sha256-digest-hi
           (-> "hi"
               as-hex)))))

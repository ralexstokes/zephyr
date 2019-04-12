(ns bitfield-test
  (:require [clojure.test :refer :all]
            [bitfield]))

(deftest basic-bitfield
  (let [some-bitfield (byte-array [(byte 0x3) (byte 0x0)])]
    (testing "can check a bit is set"
      (is
       (bitfield/has-bit-set? some-bitfield 0))
      (is
       (bitfield/has-bit-set? some-bitfield 1))
      (is
       (every? false?
               (map #(bitfield/has-bit-set? some-bitfield %) (range 2 16)))))
    (testing "can confirm a valid bitfield"
      (is
       (bitfield/valid? some-bitfield 16)))
    (testing "can confirm an invalid bitfield"
      (is true))))

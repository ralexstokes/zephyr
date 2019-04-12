(ns validator-test
  (:require [clojure.test :refer :all]
            [validator :refer :all]))

(defn make-a-validator [far-future-epoch]
  (validator/new "pubkey" "withdrawal-credentials" far-future-epoch))

(defn make-some-validators [count far-future-epoch]
  (repeat count (make-a-validator far-future-epoch)))

(deftest can-find-active-subset
  (let [far-future-epoch 100
        activation-epoch 10
        some-epoch (+ activation-epoch 2)
        validator-count 10
        active-validator-count (/ validator-count 2)
        inactive-validator-count (- validator-count active-validator-count)
        expected-inactive-validators (make-some-validators inactive-validator-count far-future-epoch)
        expected-active-validators (->> (make-some-validators active-validator-count far-future-epoch)
                                        (map #(assoc % :activation-epoch activation-epoch)))
        validator-registry (concat expected-active-validators expected-inactive-validators)
        is-active?-filter #(validator/is-active? % some-epoch)]
    (testing "can get a subset of validators based on those that are active"
      (let [active-validators (filter is-active?-filter validator-registry)
            inactive-validators (filter (complement is-active?-filter) validator-registry)]
        (is (= active-validators expected-active-validators))
        (is (= inactive-validators expected-inactive-validators))))))

(deftest can-find-slashable-subset
  (let [far-future-epoch 100
        activation-epoch 10
        withdrawable-epoch far-future-epoch
        some-epoch (+ activation-epoch 2)
        validator-count 10
        non-slashable-validator-count (/ validator-count 2)
        slashable-validator-count (- validator-count non-slashable-validator-count)
        expected-non-slashable-validators (->> (make-some-validators non-slashable-validator-count far-future-epoch)
                                               (map #(assoc % :activation-epoch activation-epoch))
                                               (map #(assoc % :slashed? true)))
        expected-slashable-validators (->> (make-some-validators slashable-validator-count far-future-epoch)
                                           (map #(assoc % :activation-epoch activation-epoch)))
        validator-registry (concat expected-non-slashable-validators expected-slashable-validators)
        is-slashable?-filter #(validator/is-slashable? % some-epoch)]
    (testing "can get a subset of validators based on those that are slashable"
      (let [slashable-validators (filter is-slashable?-filter validator-registry)
            non-slashable-validators (filter (complement is-slashable?-filter) validator-registry)]
        (is (= slashable-validators expected-slashable-validators))
        (is (= non-slashable-validators expected-non-slashable-validators))))))

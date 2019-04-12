(ns state-test
  (:require [clojure.test :refer :all]
            [state :refer :all]))

(defn make-some-state [slot]
  (-> (state/new)
      (assoc :slot slot)))

(deftest compute-epochs-from-state
  (let [slot 64
        slots-per-epoch 8
        expected-current-epoch (quot slot slots-per-epoch)
        expected-previous-epoch (dec expected-current-epoch)
        expected-next-epoch (inc expected-current-epoch)
        some-state (make-some-state slot)
        system-parameters {:slots-per-epoch slots-per-epoch}]
    (testing "can compute previous epoch"
      (is (= (state/->previous-epoch some-state system-parameters)
             expected-previous-epoch)))
    (testing "can compute current epoch"
      (is (= (state/->current-epoch some-state system-parameters)
             expected-current-epoch)))
    (testing "can compute next epoch"
      (is (= (state/->next-epoch some-state system-parameters)
             expected-next-epoch)))))

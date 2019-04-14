(ns state-transitions.entries-and-exits
  (:require [state]
            [validator]))

(defn- update-validator-entry-or-exit-if-eligible [state index validator {:keys [far-future-epoch max-deposit-amount ejection-balance] :as system-parameters}]
  (let [balance (state/balance-for-validator state index)]
    (cond
      (and (= (:activation-eligibility-epoch validator)
              far-future-epoch)
           (>= balance
               max-deposit-amount)) (validator/initiate-entry validator (state/->current-epoch state system-parameters))
      (and (validator/is-active? validator (state/->current-epoch state system-parameters))
           (< balance ejection-balance)) (validator/initiate-exit validator (state/->exit-queue-epoch state) system-parameters)
      :else validator)))

(defn process [state system-parameters]
  (update state :validator-registry
          #(map-indexed
            (fn [index validator]
              (update-validator-entry-or-exit-if-eligible state index validator system-parameters))
            %)))

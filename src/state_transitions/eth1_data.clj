(ns state-transitions.eth1-data
  (:require [state]))

(defn process [state {:keys [epochs-per-eth1-voting-period slots-per-epoch] :as system-parameters}]
  (if (zero? (mod (state/->next-epoch state system-parameters)
                  epochs-per-eth1-voting-period))
    (assoc (reduce #(if (> (* (:vote-count %2) 2)
                           (* epochs-per-eth1-voting-period slots-per-epoch))
                      (assoc %1 :latest-eth1-data (:eth1-data %2))
                      %1) state (:eth1-data-votes state))
           :eth1-data-votes
           [])
    state))

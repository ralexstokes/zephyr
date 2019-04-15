(ns state-transitions.slashings
  (:require [state]))

(defn- apply-penalty-if-slashed [state index validator current-epoch total-penalties total-balance {:keys [latest-slashed-exit-length min-penalty-quotient] :as system-parameters}]
  (if (and (:slashed? validator)
           (= current-epoch
              (- (:withdrawable-epoch validator)
                 (quot latest-slashed-exit-length 2))))
    (let [penalty (max (quot (* (state/->effective-balance state index system-parameters)
                                (min (* total-penalties 3)
                                     total-balance))
                             total-balance)
                       (quot (state/->effective-balance state index system-parameters)
                             min-penalty-quotient))]
      (state/decrease-balance state index penalty system-parameters))
    state))

(defn process [state {:keys [latest-slashed-exit-length] :as system-parameters}]
  (let [current-epoch (state/->current-epoch state system-parameters)
        next-epoch (state/->next-epoch state system-parameters)
        active-validator-indices (state/->active-validator-indices state current-epoch)
        total-balance (state/->total-balance state active-validator-indices system-parameters)
        total-at-start (get-in state [:latest-slashed-balances (mod next-epoch latest-slashed-exit-length)])
        total-at-end (get-in state [:latest-slashed-balances (mod current-epoch latest-slashed-exit-length)])
        total-penalties (- total-at-end total-at-start)]
    (reduce (fn [state [index validator]]
              (apply-penalty-if-slashed state index validator current-epoch total-penalties total-balance system-parameters)) state (map-indexed vector (:validator-registry state)))))

(ns validator
  "Contains logic for managing validators.")

(defrecord Validator
    [pubkey
     withdrawal-credentials
     activation-epoch
     exit-epoch
     withdrawable-epoch
     initiated-exit?
     slashed?
     high-balance])

(defn new [pubkey withdrawal-credentials far-future-epoch]
  (map->Validator
   {:pubkey pubkey
    :withdrawal-credentials withdrawal-credentials
    :activation-epoch far-future-epoch
    :exit-epoch far-future-epoch
    :withdrawable-epoch far-future-epoch
    :initiated-exit? false
    :slashed? false
    :high-balance 0}))

(defn is-active? [validator epoch]
  (and (<= (.activation-epoch validator)
           epoch)
       (< epoch
          (.exit-epoch validator))))

(defn is-slashable? [validator epoch]
  (and
   (and (<= (.activation-epoch validator) epoch)
        (< epoch (.withdrawable-epoch validator)))
   (not (.slashed? validator))))

(defn registry->active-indices [validator-registry epoch]
  (keep-indexed #(if (is-active? %2 epoch) %1) validator-registry))

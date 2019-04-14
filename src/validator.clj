(ns validator
  "Contains logic for managing validators.")

(defrecord Validator
    [pubkey
     withdrawal-credentials
     activation-eligibility-epoch
     activation-epoch
     exit-epoch
     withdrawable-epoch
     slashed?
     high-balance])

(defn new [pubkey withdrawal-credentials far-future-epoch]
  (map->Validator
   {:pubkey pubkey
    :withdrawal-credentials withdrawal-credentials
    :activation-eligibility-epoch far-future-epoch
    :activation-epoch far-future-epoch
    :exit-epoch far-future-epoch
    :withdrawable-epoch far-future-epoch
    :slashed? false
    :high-balance 0}))

(defn is-active? [validator epoch]
  (and (<= (:activation-epoch validator)
           epoch)
       (< epoch
          (:exit-epoch validator))))

(defn is-slashable? [validator epoch]
  (and
   (and (<= (:activation-epoch validator) epoch)
        (< epoch (:withdrawable-epoch validator)))
   (not (:slashed? validator))))

(defn initiate-entry [validator epoch]
  (update validator :activation-eligibility-epoch epoch))

(defn activate
  "from spec: activate_validator"
  [validator activation-epoch is-genesis {:keys [genesis-epoch activation-exit-delay] :as system-parameters}]
  (if is-genesis
    (-> validator
        (assoc :activation-eligibility-epoch genesis-epoch)
        (assoc :activation-epoch genesis-epoch))
    (assoc validator :activation-epoch
           (epoch/->delayed-activation-exit-epoch activation-epoch activation-exit-delay))))

(defn initiate-exit [validator exit-queue-epoch {:keys [far-future-epoch min-validator-withdrawability-delay]}]
  (if (= (:exit-epoch validator)
         far-future-epoch)
    validator
    (-> validator
        (assoc :exit-epoch exit-queue-epoch)
        (assoc :withdrawable-epoch (+ exit-queue-epoch min-validator-withdrawability-delay)))))

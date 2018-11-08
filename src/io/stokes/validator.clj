(ns io.stokes.validator
  (:require [io.stokes.hash :as hash]))

(defn create [id data]
  (assoc data ::id id))

(defn status-is [status]
  (fn [validator]
    (= (::status validator)
       status)))

(defn- indices [pred coll]
  (keep-indexed #(when (pred %2) %1) coll))

(defn- min-empty-validator [validators]
  (first (indices (status-is :withdrawn)) validators))

;; TODO sort this out
(declare bls-verify)

(defn add [validators pubkey proof-of-possession withdrawal-shard withdrawal-address randao-commitment status current-slot {:keys [deposit-size gwei-per-eth]}]
  (assert (bls-verify pubkey (hash/value pubkey) proof-of-possession))
  (let [id (min-empty-validator validators)
        validator (create id {::pubkey pubkey
                              ::withdrawal-shard withdrawal-shard
                              ::withdrawal-address withdrawal-address
                              ::randao-commitment randao-commitment
                              ::randao-last-change current-slot
                              ::balance (* deposit-size gwei-per-eth)
                              ::status status
                              ::exit-slot 0})]
    (if-let [index (min-empty-validator validators)]
      ;; TODO see if we can avoid this pair return
      [(assoc validators index validator) index]
      [(conj validators validator) (count validators)])))

;; TODO clean up
;; (defn remove [index crystallized-state penalize current-slot]
;;   (let [validator (state/nth-validator crysta)]))

(defn create-set
  [initial-validator-logs]
  #{})

(comment
  )

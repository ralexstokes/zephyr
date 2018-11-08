(ns io.stokes.validator
  (:require
   [io.stokes.hash :as hash]
   [io.stokes.shuffling :as shuffling]))

(defn create [id data]
  (-> data
      (assoc ::id id)
      ;; TODO fix wrong status, in general
      (assoc ::status :active)))

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

(defn- clamp [min max value]
  (cond
    (<= value min) min
    (>= value max) max
    :else value))

(defn- calculate-committes-per-slot [shard-count cycle-length active-validator-count min-committee-size]
  (clamp 1 (quot shard-count cycle-length)
         (quot
          (quot active-validator-count
                cycle-length)
          (+ (* min-committee-size 2) 1))))

(def allocate-validators-to-slots shuffling/split-into-pieces)

(defn- allocate-validators-in-each-slot-to-committees [validators-by-slot committees-per-slot current-shard shard-count]
  (map-indexed
   (fn [slot-offset validators]
     (let [validators-by-shard (shuffling/split-into-pieces validators committees-per-slot)
           starting-shard-for-this-committee (+ current-shard (* slot-offset
                                                                 committees-per-slot))]
       (map-indexed
        (fn [shard-offset validators]
          {::shard (mod (+ starting-shard-for-this-committee shard-offset)
                        shard-count)
           ::committee validators})
        validators-by-shard)))
   validators-by-slot))

(defn new-shuffling-to-slots-and-committees [validators seed current-shard {:keys [shard-count cycle-length min-committee-size]}]
  (let [active-validators (filter (status-is :active) validators)
        committees-per-slot (calculate-committes-per-slot
                             shard-count
                             cycle-length
                             (count active-validators)
                             min-committee-size)]
    (-> active-validators
        (shuffling/with-seed seed)
        (allocate-validators-to-slots cycle-length)
        (allocate-validators-in-each-slot-to-committees committees-per-slot current-shard shard-count))))

(comment
  )

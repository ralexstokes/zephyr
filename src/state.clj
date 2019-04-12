(ns state
  "Contains facilities for dealing with the Eth2.0 state. Note that each forks respective state transition may contain additional semantics around handling the state."
  (:require [clojure.set :as set]
            [validator]
            [attestation]
            [crosslink]
            [slot]
            [hash]
            [bitfield]
            [epoch]
            [block]
            [bls]
            [ssz]
            [committee]))

(defrecord BeaconState
    [slot
     genesis-time
     fork
     validator-registry
     balances
     validator-registry-update-epoch
     latest-randao-mixes
     latest-start-shard
     previous-epoch-attestations
     current-epoch-attestations
     previous-justified-epoch
     current-justified-epoch
     previous-justified-root
     current-justified-root
     justification-bitfield
     finalized-epoch
     finalized-root
     latest-crosslinks
     latest-block-roots
     latest-state-roots
     latest-active-index-roots
     latest-slashed-balances
     latest-block-header
     historical-roots
     latest-eth1-data
     eth1-data-votes
     deposit-index])

(defn new [genesis-time genesis-eth1-data {:keys [genesis-slot genesis-fork-version genesis-epoch latest-randao-mixes-length genesis-start-shard shard-count slots-per-historical-root latest-active-index-roots-length latest-slashed-exit-length]}]
  (map->BeaconState
   {:slot genesis-slot
    :genesis-time genesis-time
    :fork (fork/new genesis-epoch
                    genesis-fork-version
                    genesis-fork-version)
    :validator-registry []
    :balances []
    :validator-registry-update-epoch genesis-epoch
    :latest-randao-mixes (into [] (repeat latest-randao-mixes-length hash/zero))
    :latest-start-shard genesis-start-shard
    :previous-epoch-attestations []
    :current-epoch-attestations []
    :previous-justified-epoch (- genesis-epoch 1)
    :current-justified-epoch genesis-epoch
    :previous-justified-root hash/zero
    :current-justified-root hash/zero
    :justification-bitfield (bitfield/new)
    :finalized-epoch genesis-epoch
    :finalized-root hash/zero
    :latest-crosslinks (into [] (repeat shard-count (crosslink/new genesis-epoch hash/zero)))
    :latest-block-roots (into [] (repeat slots-per-historical-root hash/zero))
    :latest-state-roots (into [] (repeat slots-per-historical-root hash/zero))
    :latest-active-index-roots (into [] (repeat latest-active-index-roots-length hash/zero))
    :latest-slashed-balances (into [] (repeat latest-slashed-exit-length 0))
    :latest-block-header (block/->temporary-block-header (block/empty genesis-slot))
    :historical-roots []
    :latest-eth1-data genesis-eth1-data
    :eth1-data-votes []
    :deposit-index 0}))

(defn ->current-epoch [state {:keys [slots-per-epoch]}]
  (-> state
      .slot
      (slot/->epoch slots-per-epoch)))

(defn ->previous-epoch [state system-parameters]
  (-> state
      (->current-epoch system-parameters)
      dec))

(defn ->next-epoch [state system-parameters]
  (-> state
      (->current-epoch system-parameters)
      inc))

(defn balance-for-validator
  "from spec: get_balance"
  [state index]
  (-> state
      .balances
      (nth index)))

(defn validator-at-index [state index]
  (-> state
      .validator-registry
      (nth index)))

(defn- increment-validator-high-balance [validator balance high-balance-increment]
  (assoc validator :high-balance (- balance
                                    (mod balance high-balance-increment))))

(defn update-balance-for-validator
  "from spec: set_balance"
  [state index balance {:keys [high-balance-increment]}]
  (let [validator (validator-at-index state index)
        half-increment (quot high-balance-increment 2)
        validator-high-balance (.high-balance validator)
        should-adjust-high-balance? (or (> validator-high-balance
                                           balance)
                                        (< (+ validator-high-balance
                                              (* 3 half-increment))
                                           balance))
        validator-registry (.validator-registry state)
        new-validator-registry (if should-adjust-high-balance?
                                 (assoc validator-registry index
                                        (increment-validator-high-balance validator balance high-balance-increment))
                                 validator-registry)
        new-balances (assoc (.balances state) index balance)]
    (merge state {:validator-registry new-validator-registry
                  :balances new-balances})))

(defn increase-balance [state index delta]
  (update-balance-for-validator state index
                                (+ (balance-for-validator state index) delta)))

(defn decrease-balance [state index delta system-parameters]
  (let [current-balance (balance-for-validator state index)
        next-balance (if (>= current-balance delta)
                       (- current-balance delta)
                       0)]
    (update-balance-for-validator state index next-balance system-parameters)))

(defn ->shard-delta [state epoch {:keys [shard-count slots-per-epoch] :as system-parameters}]
  (min (committee/count-for-epoch state epoch system-parameters)
       (- shard-count
          (quot shard-count slots-per-epoch))))

(declare generate-seed)

(defn- compute-start-shard [state epoch previous-epoch current-epoch next-epoch committees-per-epoch {:keys [shard-count] :as system-parameters}]
  (let [latest-start-shard (.latest-start-shard state)]
    (cond
      (= epoch current-epoch) latest-start-shard
      (= epoch previous-epoch) (let [previous-shard-delta (->shard-delta state previous-epoch system-parameters)]
                                 (mod (- latest-start-shard previous-shard-delta) shard-count))
      (= epoch next-epoch) (let [current-shard-delta (->shard-delta state current-epoch system-parameters)]
                             (mod (+ latest-start-shard current-shard-delta) shard-count)))))

(defn- -compute-crosslink-committees-at-slot [state
                                              slot
                                              epoch
                                              current-epoch
                                              previous-epoch
                                              next-epoch
                                              {:keys [shard-count slots-per-epoch] :as system-parameters}]
  (let [indices (validator/registry->active-indices (.validator-registry state) epoch)
        committees-per-epoch (committee/count-for-epoch state epoch system-parameters)
        start-shard (compute-start-shard
                     state
                     epoch
                     previous-epoch
                     current-epoch
                     next-epoch
                     committees-per-epoch
                     system-parameters)
        committees-per-slot (quot committees-per-epoch slots-per-epoch)
        offset (mod slot slots-per-epoch)
        slot-start-shard (mod (+ start-shard (* committees-per-slot offset)) shard-count)
        seed (generate-seed state epoch system-parameters)]
    (map #(vector
           (committee/at-index indices seed (+ (* committees-per-slot offset) %) committees-per-epoch)
           (mod (+ slot-start-shard %) shard-count)) (range committees-per-slot))))

(defn ->crosslink-committees-at-slot [state slot system-parameters]
  (let [epoch (slot/->epoch slot)
        current-epoch (->current-epoch state system-parameters)
        previous-epoch (->previous-epoch state system-parameters)
        next-epoch (->next-epoch state system-parameters)]
    (if (or (> previous-epoch epoch)
            (> epoch next-epoch))
      (throw (ex-info "epoch must be no earlier than previous epoch and no later than the next epoch" {:previous-epoch previous-epoch
                                                                                                       :epoch epoch
                                                                                                       :next-epoch next-epoch}))
      (-compute-crosslink-committees-at-slot state slot epoch current-epoch previous-epoch next-epoch system-parameters))))

(defn ->block-root [state slot {:keys [slots-per-historical-root]}]
  (let [state-slot (.slot state)
        index (mod slot slots-per-historical-root)]
    (if (or (>= slot state-slot)
            (> state-slot (+ slot slots-per-historical-root)))
      (throw (ex-info "slot must be prior to the current slot and within the bounds of the latest block roots"
                      {:slot slot
                       :state-slot state-slot}))
      (-> state
          .latest-block-roots
          (nth index)))))

(defn ->state-root [state slot {:keys [slots-per-historical-root]}]
  (let [state-slot (.slot state)
        index (mod slot slots-per-historical-root)]
    (if (or (>= slot state-slot)
            (> state-slot (+ slot slots-per-historical-root)))
      (throw (ex-info "slot must be prior to the current slot and within the bounds of the latest block roots"
                      {:slot slot
                       :state-slot state-slot}))
      (-> state
          .latest-state-roots
          (nth index)))))

(defn ->randao-mix [state epoch {:keys [latest-randao-mixes-length :as system-parameters]}]
  (let [current-epoch (->current-epoch state system-parameters)
        lower-bound (- current-epoch latest-randao-mixes-length)
        upper-bound current-epoch
        index (mod epoch latest-randao-mixes-length)]
    (if (or (>= lower-bound epoch)
            (> epoch upper-bound))
      (throw (ex-info "epoch must be within bounds for randao access"
                      {:current-epoch current-epoch
                       :lower-bound lower-bound
                       :upper-bound upper-bound
                       :epoch epoch}))
      (-> state
          .latest-randao-mixes
          (nth index)))))

(defn ->active-index-root [state epoch {:keys [latest-active-index-roots-length activation-exit-delay slots-per-epoch] :as system-parameters}]
  (let [current-epoch (->current-epoch state system-parameters)
        lower-bound (- current-epoch (+ latest-active-index-roots-length activation-exit-delay))
        upper-bound (+ current-epoch activation-exit-delay)
        index (mod epoch latest-active-index-roots-length)]
    (if (or (>= lower-bound epoch)
            (> epoch upper-bound))
      (throw (ex-info "epoch must be within bounds for randao access"
                      {:current-epoch current-epoch
                       :lower-bound lower-bound
                       :upper-bound upper-bound
                       :epoch epoch}))
      (-> state
          .latest-active-index-roots
          (nth index)))))

(defn generate-seed [state epoch {:keys [min-seed-lookahead]}]
  (hash/of
   (->randao-mix state (- epoch min-seed-lookahead))
   (->active-index-root state epoch)
   (bytes/from-int32 epoch)))

(defn ->effective-balance [state index {:keys [max-deposit-amount]}]
  (min (balance-for-validator state index)
       max-deposit-amount))

(defn- valid-proposer-candidate? [state candidate rand-byte {:keys [max-deposit-amount] :as system-parameters}]
  (> (* (->effective-balance state candidate system-parameters)
        256)
     (* max-deposit-amount
        rand-byte)))

(defn- -find-beacon-proposer-index [state epoch committee {:keys [max-deposit-amount] :as system-parameters}]
  (let [count-committee (count committee)]
    (loop [i 0]
      (let [rand-hash (hash/of
                       (bytes/join
                        (generate-seed state epoch system-parameters)
                        bytes/from-int8 (quot i 32)))
            rand-byte (nth rand-hash (mod i 32))
            candidate (nth committee (mod (+ epoch i) count-committee))]
        (if (valid-proposer-candidate? state candidate rand-byte system-parameters)
          candidate
          (recur (inc i)))))))

(defn ->beacon-proposer-index [state slot {:keys [slots-per-epoch] :as system-parameters}]
  (let [current-epoch (->current-epoch state system-parameters)
        epoch (slot/->epoch slot slots-per-epoch)
        first-committee (first (first (->crosslink-committees-at-slot state slot system-parameters)))]
    (if (not= current-epoch epoch)
      (throw (ex-info "can only get the proposer for the current epoch"
                      {:current-epoch current-epoch
                       :requested-epoch epoch}))
      (-find-beacon-proposer-index state current-epoch first-committee system-parameters))))

(defn ->crosslink-committee-for-attestation [state attestation-data system-parameters]
  (let [committee (->> (->crosslink-committees-at-slot state (.slot attestation-data) system-parameters)
                       (filter (fn [committee shard] (= shard (.shard attestation-data))))
                       first
                       first)]
    (if committee
      committee
      (throw (ex-info "invalid attestation-data: no shard at the claimed slot"
                      {:attestation-data attestation-data})))))

(defn ->attestation-participants [state attestation-data bitfield system-parameters]
  (let [committee (->crosslink-committee-for-attestation state attestation-data system-parameters)]
    (if (bitfield/valid? bitfield (count committee))
      (sort (keep-indexed #(if (bitfield/has-bit-set? bitfield %1) %2) committee))
      (throw (ex-info "invalid bitfield"
                      {:bitfield bitfield
                       :expected-length (count committee)})))))

(defn ->total-balance [state validator-indices system-parameters]
  (->> validator-indices
       (map #(->effective-balance state % system-parameters))
       (reduce +)))

(defn convert-to-indexed [state attestation system-parameters]
  (let [attesting-indices (->attestation-participants state (.data attestation) (.aggregation-bitfield attestation) system-parameters)
        custody-bit-1-indices (->attestation-participants state (.data attestation) (.custody-bitfield attestation) system-parameters)
        custody-bit-0-indices (filter (complement (set custody-bit-1-indices)) attesting-indices)]
    (attestation/map->IndexedAttestation {:custody-bit-0-indices custody-bit-0-indices
                                          :custody-bit-1-indices custody-bit-1-indices
                                          :data (.data attestation)
                                          :aggregate-signature (.aggregate-signature attestation)})))

(defn- has-disjoint-custody-bits-set? [bit-0-set bit-1-set]
  (empty?
   (set/intersection bit-0-set bit-1-set)))

(defn- has-correct-number-of-participants? [number-participants max-number-participants]
  (and
   (<= 1 number-participants)
   (<= number-participants max-number-participants)))

(defn- indices-are-sorted? [indices]
  (= (sort indices)
     indices))

(defn- aggregate-pubkey-for-indices [state indices]
  (let [indices (set indices)
        validators (keep-indexed #(if (indices %1) %2) (.validator-registry state))]
    (bls/aggregate-pubkeys (map #(.pubkey %) validators))))

(defn- build-message-hash-for-attestation [indexed-attestation custody-bit]
  (ssz/hash-tree-root
   (attestation/map->AttestationDataAndCustodyBit
    {:data (.data indexed-attestation)
     :custody-bit custody-bit})))

(defn is-valid-indexed-attestation? [state indexed-attestation {:keys [max-attestation-participants domain-attestation]}]
  (let [custody-bit-0-indices (.custody-bit-0-indices indexed-attestation)
        custody-bit-1-indices (.custody-bit-1-indices indexed-attestation)
        number-participants (+ (count custody-bit-0-indices) (count custody-bit-1-indices))]
    (and
     (has-disjoint-custody-bits-set? custody-bit-0-indices custody-bit-1-indices)
     (zero? (count custody-bit-1-indices))
     (has-correct-number-of-participants? number-participants max-attestation-participants)
     (indices-are-sorted? custody-bit-0-indices)
     (indices-are-sorted? custody-bit-1-indices)
     (bls/multiple-valid?
      [(aggregate-pubkey-for-indices state custody-bit-0-indices)
       (aggregate-pubkey-for-indices state custody-bit-1-indices)]
      [(build-message-hash-for-attestation indexed-attestation false)
       (build-message-hash-for-attestation indexed-attestation true)]
      (.aggregate-signature indexed-attestation)
      (.fork state)
      (-> indexed-attestation .data .slot)
      domain-attestation))))

(defn activate-validator [state index is-genesis {:keys [genesis-epoch activation-exit-delay] :as system-parameters}]
  (update-in state [:validator-registry index :activation-epoch]
             (fn [_] (if is-genesis
                       genesis-epoch
                       (epoch/->delayed-activation-exit-epoch (->current-epoch state system-parameters) activation-exit-delay)))))

(defn initiate-validator-exit [state index]
  (update-in state [:validator-registry index :initiated-exit?] not))

(defn exit-validator [state index {:keys [far-future-epoch slots-per-epoch activation-exit-delay] :as system-parameters}]
  (update-in state [:validator-registry index :exit-epoch]
             (fn [exit-epoch]
               (if (= exit-epoch far-future-epoch)
                 (epoch/->delayed-activation-exit-epoch (->current-epoch state system-parameters) activation-exit-delay)
                 exit-epoch))))

(defn slash-validator
  ([state slashed-index system-parameters] (slash-validator state slashed-index nil system-parameters))
  ([state slashed-index whistleblower-index {:keys [latest-slashed-exit-length whistleblowing-reward-quotient proposer-reward-quotient] :as system-parameters}]
   (let [current-epoch (->current-epoch state system-parameters)
         proposer-index (->beacon-proposer-index state (.slot state) system-parameters)
         slashed-balance (->effective-balance state slashed-index system-parameters)
         whistleblower-index (or whistleblower-index proposer-index)
         whistleblowing-reward (quot slashed-balance whistleblowing-reward-quotient)
         proposer-reward (quot whistleblowing-reward proposer-reward-quotient)]
     (-> state
         (exit-validator slashed-index system-parameters)
         (update-in [:validator-registry slashed-index :slashed?] not)
         (update-in [:validator-registry slashed-index :withdrawable-epoch] (fn [_] (+ current-epoch latest-slashed-exit-length)))
         (update-in [:latest-slashed-balances (mod current-epoch latest-slashed-exit-length)] #(+ % slashed-balance))
         (increase-balance state proposer-index proposer-reward)
         (increase-balance state whistleblower-index (- whistleblowing-reward proposer-reward))
         (decrease-balance state slashed-index whistleblowing-reward system-parameters)))))

(defn prepare-validator-for-withdrawal [state index {:keys [min-validator-withdrawability-delay] :as system-parameters}]
  (update-in [:validator-registry index :withdrawable-epoch] (fn [_] (+ (->current-epoch state system-parameters)
                                                                        min-validator-withdrawability-delay))))

(ns state-transitions.rewards
  (:require [state]
            [math]))

(defn- ->base-reward [state index {:keys [base-reward-quotient] :as system-parameters}]
  (if (zero? (state/->previous-total-balance state system-parameters))
    0
    (quot (state/->effective-balance state index system-parameters)
          (quot (quot (math/integer-squareroot (state/->previous-total-balance state system-parameters))
                      base-reward-quotient)
                5))))

(defn- ->inactivity-penalty [state index epochs-since-finality {:keys [inactivity-penalty-quotient] :as system-parameters}]
  (+ (->base-reward state index system-parameters)
     (if (<= epochs-since-finality 4)
       0
       (quot (quot (* (state/->effective-balance state index system-parameters)
                      epochs-since-finality)
                   inactivity-penalty-quotient)
             2))))

(defn- compute-rewards-for-ffg-source [index state base-reward total-attesting-balance total-balance {:keys [min-attestation-inclusion-delay] :as system-parameters}]
  (let [rewardable-indices (state/->unslashed-attesting-indices state (:previous-epoch-attestations state) system-parameters)
        set-of-rewardable-indices (into #{} rewardable-indices)]
    {:index index
     :amount (if (set-of-rewardable-indices index)
               (+ (quot (* base-reward
                           total-attesting-balance)
                        total-balance)
                  (quot (* base-reward
                           min-attestation-inclusion-delay)
                        (state/->inclusion-distance state index system-parameters)))
               (- base-reward))}))

(defn- compute-rewards-for-ffg-target [index state base-reward total-balance boundary-attestations boundary-attesting-balance epochs-since-finality system-parameters]
  (let [rewardable-indices (state/->unslashed-attesting-indices state boundary-attestations system-parameters)
        set-of-rewardable-indices (into #{} rewardable-indices)]
    {:index index
     :amount (if (set-of-rewardable-indices index)
               (quot (* base-reward boundary-attesting-balance)
                     total-balance)
               (- (->inactivity-penalty state index epochs-since-finality system-parameters)))}))

(defn- compute-rewards-for-beacon-head [index state base-reward total-balance matching-head-attestations matching-head-balance system-parameters]
  (let [rewardable-indices (state/->unslashed-attesting-indices state matching-head-attestations system-parameters)
        set-of-rewardable-indices (into #{} rewardable-indices)]
    {:index index
     :amount (if (set-of-rewardable-indices index)
               (quot (* base-reward matching-head-balance)
                     total-balance)
               (- base-reward))}))

(defn- compute-rewards-for-proposer [index state base-reward {:keys [proposer-reward-quotient] :as system-parameters}]
  (let [rewardable-indices (state/->unslashed-attesting-indices state (:previous-epoch-attestations state) system-parameters)
        set-of-rewardable-indices (into #{} rewardable-indices)]
    (if (set-of-rewardable-indices index)
      {:index (state/->beacon-proposer-index state (state/->inclusion-slot state index system-parameters) system-parameters)
       :amount (quot base-reward
                     proposer-reward-quotient)})))

(defn- compute-rewards-for-lack-of-finality [index base-reward epochs-since-finality]
  (if (> epochs-since-finality 4)
    {:index index
     :amount (- (* base-reward 4))}))

(defn- merge-deltas [{:keys [index1 amount1]} {:keys [index2 amount2]}]
  (assert (= index1 index2))
  {:index index1
   :amount (+ amount1 amount2)})

(defn- ->justification-and-finalization-deltas [state system-parameters]
  (let [current-epoch (state/->current-epoch state system-parameters)
        next-epoch (state/->next-epoch state system-parameters)
        epochs-since-finality (- next-epoch (:finalized-epoch state))
        boundary-attestations (state/->previous-epoch-boundary-attestations state system-parameters)
        boundary-attesting-balance (state/->attesting-balance state boundary-attestations system-parameters)
        total-balance (state/->previous-total-balance state system-parameters)
        total-attesting-balance (state/->attesting-balance state (:previous-epoch-attestations state))
        matching-head-attestations (state/->previous-epoch-matching-head-attestations state system-parameters)
        matching-head-balance (state/->attesting-balance state matching-head-attestations)
        eligible-validator-indices (->> (:validator-registry state)
                                        (map-indexed #(vector %1 %2))
                                        (filter (fn [[index validator]]
                                                  (or (validator/is-active? validator current-epoch)
                                                      (and (:slashed? validator)
                                                           (< current-epoch
                                                              (:withdrawable-epoch validator))))))
                                        (map (fn [[index _]] index)))]
    (map (fn [index]
           (let [base-reward (->base-reward state index system-parameters)]
             (reduce merge-deltas [(compute-rewards-for-ffg-source index state base-reward total-attesting-balance total-balance system-parameters)
                                   (compute-rewards-for-ffg-target index state base-reward total-balance boundary-attestations boundary-attesting-balance epochs-since-finality system-parameters)
                                   (compute-rewards-for-beacon-head index state base-reward total-balance matching-head-attestations matching-head-balance system-parameters)
                                   (compute-rewards-for-proposer index state base-reward system-parameters)
                                   (compute-rewards-for-lack-of-finality index base-reward epochs-since-finality)])))
         eligible-validator-indices)))

(defn- compute-crosslink-reward-for-committee [state committee shard system-parameters]
  (let [[winning-root participants] (state/->winning-root-and-participants state shard system-parameters)
        set-of-participants (into #{} participants)
        participating-balance (state/->total-balance state participants system-parameters)
        total-balance (state/->total-balance state committee system-parameters)]
    (map #(hash-map :index %
                    :amount (if (set-of-participants %)
                              (quot (* (->base-reward state % system-parameters)
                                       participating-balance)
                                    total-balance)
                              (- (->base-reward state % system-parameters))))
         committee)))

(defn- ->crosslink-deltas [state {:keys [slots-per-epoch] :as system-parameters}]
  (let [start-slot (epoch/->start-slot (state/->previous-epoch state system-parameters) slots-per-epoch)
        end-slot (epoch/->start-slot (state/->current-epoch state system-parameters) slots-per-epoch)]
    (->> (range start-slot end-slot)
         ;; [ 0 1 ... 9 10 ]
         (mapcat #(state/->crosslink-committees-at-slot state % system-parameters))
         ;; [ '(slots * cps) (com, shard) ]
         ;; [ '(slots * cps) reward-description]
         (map (fn [[committee shard]]
                (compute-crosslink-reward-for-committee state committee shard system-parameters))))))

(defn- apply-rewards [state deltas system-parameters]
  (reduce (fn [state delta]
            (state/update-balance-for-validator
             state
             (:index delta)
             (max 0
                  (+ (state/balance-for-validator state (:index delta)) (:amount delta)))
             system-parameters))
          state
          deltas))

(defn process [state system-parameters]
  (let [justification-deltas (->justification-and-finalization-deltas state system-parameters)
        crosslink-deltas (->crosslink-deltas state system-parameters)
        deltas (concat justification-deltas crosslink-deltas)]
    (apply-rewards state deltas system-parameters)))

(comment
  )

(ns genesis
  "Contains functionality pertaining to the genesis event of Eth2.0."
  (:require [state]
            [ssz]
            [state-transitions.serenity :as serenity]
            [block]))

(defn- activate-if-max-deposit [state validator-index {:keys [max-deposit-amount] :as  system-parameters}]
  (if (>= (state/->effective-balance state validator-index system-parameters)
          max-deposit-amount)
    (state/activate-validator state validator-index true system-parameters)
    state))

(defn new-state
  "from spec: get_genesis_beacon_state"
  [genesis-deposits genesis-time genesis-eth1-data {:keys [genesis-epoch latest-active-index-roots-length] :as system-parameters}]
  (let [state (state/new genesis-time genesis-eth1-data system-parameters)
        state-with-deposits (reduce
                             #(serenity/process-deposit %1 %2 system-parameters)
                             state
                             genesis-deposits)
        state-with-activations (reduce
                                #(activate-if-max-deposit %1 %2 system-parameters)
                                state-with-deposits
                                (range (.validator-registry state-with-deposits)))
        genesis-active-index-root (ssz/hash-tree-root (validator/registry->active-indices (.validator-registry state-with-activations) genesis-epoch))]
    (update state-with-activations :latest-active-index-roots (into [] (repeat latest-active-index-roots-length genesis-active-index-root)))))

(defn new-block [genesis-state-root]
  (assoc (block/empty) :state-root genesis-state-root))

(defn create-state-and-block
  "Returns a pair consisting of the genesis (state, block)."
  [genesis-deposits genesis-time genesis-eth1-data system-parameters]
  (let [genesis-state (new-state genesis-deposits genesis-time genesis-eth1-data system-parameters)
        genesis-block (new-block (ssz/hash-tree-root genesis-state))]
    [genesis-state genesis-block]))

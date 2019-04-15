(ns state-transitions.per-epoch
  (:require [state]
            [historical-batch]
            [ssz]
            [ssz.schemas :as schemas]
            [state-transitions.justification-and-finalization :as justification]
            [state-transitions.crosslinks :as crosslinks]
            [state-transitions.eth1-data :as eth1-data]
            [state-transitions.rewards :as rewards]
            [state-transitions.entries-and-exits :as entries-and-exits]
            [state-transitions.validator-registry :as validator-registry]
            [state-transitions.slashings :as slashings]))

(defn- finalize-epoch [state {:keys [activation-exit-delay latest-active-index-roots-length latest-slashed-exit-length latest-randao-mixes-length slots-per-historical-root slots-per-epoch] :as system-parameters}]
  (let [current-epoch (state/->current-epoch state system-parameters)
        next-epoch (state/->next-epoch state system-parameters)
        index-root-index (mod (+ next-epoch
                                 activation-exit-delay)
                              latest-active-index-roots-length)
        latest-active-index-root (ssz/hash-tree-root (state/->active-validator-indices state (+ next-epoch activation-exit-delay)) schemas/list)
        slashed-balances-index (mod next-epoch latest-slashed-exit-length)
        latest-slashed-balance (get-in state [:latest-slashed-balances (mod current-epoch latest-slashed-exit-length)])
        randao-mixes-index (mod next-epoch latest-randao-mixes-length)
        randao-mix (state/->randao-mix state current-epoch system-parameters)]
    (-> state
        (assoc-in [:latest-active-index-roots index-root-index] latest-active-index-root)
        (assoc-in [:latest-slashed-balances slashed-balances-index] latest-slashed-balance)
        (assoc-in [:latest-randao-mixes randao-mixes-index] randao-mix)
        (#(if (zero? (mod next-epoch
                          (quot slots-per-historical-root slots-per-epoch)))
            (update % :historical-roots conj (ssz/hash-tree-root (historical-batch/new (:latest-block-roots %) (:latest-state-roots %)) schemas/historical-batch))
            %))
        (assoc :previous-epoch-attestations (:current-epoch-attestations state))
        (assoc :current-epoch-attestations []))))

(defn run-transition [state system-parameters]
  (-> state
      (#(justification/process % system-parameters))
      (#(crosslinks/process % system-parameters))
      (#(eth1-data/process % system-parameters))
      (#(rewards/process % system-parameters))
      ;; TODO can the next two be merged?
      (#(entries-and-exits/process % system-parameters))
      (#(validator-registry/process % system-parameters))
      (#(slashings/process % system-parameters))
      (#(finalize-epoch % system-parameters))))

(defn transition [state {:keys [genesis-slot slots-per-epoch] :as system-parameters}]
  (if (zero?
       (mod (inc (:slot state)) slots-per-epoch))
    (run-transition state system-parameters)))

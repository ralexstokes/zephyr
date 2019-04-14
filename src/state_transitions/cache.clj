(ns state-transitions.cache
  (:require [ssz]
            [ssz.schemas :as schemas]
            [hash]))

(defn- update-latest-block-roots [state slots-per-historical-root]
  (let [latest-block-root (ssz/signing-root (.latest-block-header state) schemas/beacon-block-header)]
    (update-in state [:latest-block-roots (mod (.slot state) slots-per-historical-root)] latest-block-root)))

(defn transition [state {:keys [slots-per-historical-root]}]
  (let [previous-slot-state-root (ssz/hash-tree-root state schemas/beacon-state)]
    (-> state
        (assoc-in [:latest-state-roots (mod (.slot state) slots-per-historical-root)] previous-slot-state-root)
        (update-in [:latest-block-header :state-root] (fn [existing-state-root] (if (= existing-state-root hash/zero)
                                                                                  previous-slot-state-root
                                                                                  existing-state-root)))
        (#(update-latest-block-roots % slots-per-historical-root)))))

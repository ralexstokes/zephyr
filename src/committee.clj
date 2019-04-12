(ns committee
  (:require [shuffling]))

(defn count-for-epoch
  "from spec: get_epoch_committee_count"
  [state epoch {:keys [shard-count slots-per-epoch target-committee-size]}]
  (let [active-validator-count (count (.validator-registry state))]
    (* (max 1
            (min (quot shard-count slots-per-epoch)
                 (quot (quot active-validator-count slots-per-epoch)
                       target-committee-size)))
       slots-per-epoch)))

(defn at-index
  "from spec: compute_committee"
  [validator-indices seed index total-committees]
  (let [validator-count (count validator-indices)
        start-offset (shuffling/get-split-offset validator-count total-committees index)
        end-offset (shuffling/get-split-offset validator-count total-committees (inc  index))
        permuted-indices (set (map #(shuffling/permuted-index % validator-count seed) (range start-offset end-offset)))]
    (keep-indexed #(when (permuted-indices %1) %2) validator-indices)))

(ns historical-batch)

(defrecord HistoricalBatch [block-roots state-roots])

(defn new [block-roots state-roots]
  (map->HistoricalBatch
   {:block-roots block-roots
    :state-roots state-roots}))

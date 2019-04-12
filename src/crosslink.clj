(ns crosslink)

(defrecord Crosslink [epoch crosslink-data-root])

(defn new [epoch crosslink-data-root]
  (map->Crosslink
   {:epoch epoch
    :crosslink-data-root crosslink-data-root}))

(ns state-transitions.crosslinks
  (:require [state]
            [crosslink]
            [slot]
            [epoch]))

(defn- compute-new-crosslinks [state {:keys [genesis-epoch slots-per-epoch max-crosslink-epochs] :as system-parameters}]
  (let [start-slot (epoch/->start-slot (max (state/->previous-epoch state system-parameters)
                                            genesis-epoch) system-parameters)
        end-slot (epoch/->start-slot (state/->next-epoch state system-parameters) system-parameters)]
    (->> (mapcat
          #(let [crosslink-committees (state/->crosslink-committees-at-slot state % system-parameters)]
             (map
              (fn [[committee shard]]
                (let [[winning-root participants] (state/->winning-root-and-participants state shard system-parameters)
                      participating-balance (state/->total-balance state participants system-parameters)
                      total-balance (state/->total-balance state committee system-parameters)]
                  {:slot %
                   :shard shard
                   :winning-root winning-root
                   :met-threshold? (>= (* participating-balance 3)
                                       (* total-balance 2))}))
              crosslink-committees))
          (range start-slot end-slot))
         (filter :met-threshold?)
         (map (fn [{:keys [slot shard winning-root]}] {:shard shard :crosslink (crosslink/new (min (slot/->epoch slot slots-per-epoch)
                                                                                                   (+ (get-in state [:latest-crosslinks shard :epoch])
                                                                                                      max-crosslink-epochs))
                                                                                              winning-root)})))))

(defn- update-crosslink [state {:keys [shard crosslink]}]
  (assoc-in state [:latest-crosslinks shard] crosslink))

(defn process [state system-parameters]
  (reduce update-crosslink state (compute-new-crosslinks state system-parameters)))

(ns state-transitions.validator-registry
  (:require [state]
            [epoch]))

(defn- merge-validators [validator-registry [index validator]]
  (assoc validator-registry index validator))

(defn- process-activation-queue [state {:keys [far-future-epoch activation-exit-delay] :as system-parameters}]
  (update state :validator-registry
          (fn [validator-registry]
            (let [activated-validators (->> (keep-indexed #(if (and (not= (:activation-eligibility-epoch %2)
                                                                          far-future-epoch)
                                                                    (>= (:activation-epoch %2)
                                                                        (epoch/->delayed-activation-exit-epoch (:finalized-epoch state) activation-exit-delay)))
                                                             [%1 %2])
                                                          validator-registry)
                                            (sort-by (comp :activation-eligibility-epoch second))
                                            (take (state/->churn-limit state system-parameters))
                                            (map #(update % 1 (fn [validator]
                                                                (validator/activate validator (state/->current-epoch state system-parameters) false system-parameters)))))]
              (reduce merge-validators validator-registry activated-validators)))))

(defn- update-latest-start-shard [state {:keys [shard-count] :as system-parameters}]
  (update state :latest-start-shard #(mod (+ %
                                             (state/->shard-delta state (state/->current-epoch state system-parameters) system-parameters))
                                          shard-count)))

(defn process
  "from spec: update_registry"
  [state system-parameters]
  (-> state
      (#(process-activation-queue % system-parameters))
      (#(update-latest-start-shard % system-parameters))))

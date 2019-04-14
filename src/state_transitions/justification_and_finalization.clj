(ns state-transitions.justification-and-finalization)

(defn- compute-justification-for-previous-epoch [state system-parameters]
  (let [previous-boundary-attesting-balance (state/->attesting-balance state (state/->previous-epoch-boundary-attestations state system-parameters) system-parameters)]
    (if (>= (* previous-boundary-attesting-balance 3)
            (* (state/->previous-total-balance state system-parameters) 2))
      {:new-justified-epoch (state/->previous-epoch state system-parameters)
       :bitfield-mask 2})))

(defn- compute-justification-for-current-epoch [state system-parameters]
  (let [current-boundary-attesting-balance (state/->attesting-balance state (state/->current-epoch-boundary-attestations state system-parameters) system-parameters)]
    (if (>= (* current-boundary-attesting-balance 3)
            (* (state/->current-total-balance state system-parameters) 2))
      {:new-justified-epoch (state/->current-epoch state system-parameters)
       :bitfield-mask 1})))

(defn- merge-justification-result [{:keys [new-justified-epoch new-justification-bitfield] :as accumulator} result]
  (if result
    (let [{:keys [new-justified-epoch bitfield-mask]} result]
      {:new-justified-epoch new-justified-epoch
       :new-justification-bitfield (bit-or new-justification-bitfield bitfield-mask)})
    accumulator))

(defn- determine-new-justifications
  [state system-parameters]
  (let [previous-epoch-result (compute-justification-for-previous-epoch state system-parameters)
        current-epoch-result (compute-justification-for-current-epoch state system-parameters)]
    (reduce merge-justification-result
            {:new-justified-epoch (:current-justified-epoch state)
             :new-justification-bitfield (:justification-bitfield state)}
            [previous-epoch-result current-epoch-result])))

(defn- determine-new-finality [state bitfield system-parameters]
  (let [current-epoch (state/->current-epoch state system-parameters)]
    (-> (:finalized-epoch state)
        (#(if (and (= (mod (unsigned-bit-shift-right bitfield 1) 8)
                      2r111)
                   (= (:previous-justified-epoch state)
                      (- current-epoch 3)))
            (:previous-justified-epoch state)
            %))
        (#(if (and (= (mod (unsigned-bit-shift-right bitfield 1) 4)
                      2r11)
                   (= (:previous-justified-epoch state)
                      (- current-epoch 2)))
            (:previous-justified-epoch state)
            %))
        (#(if (and (= (mod bitfield 8)
                      2r111)
                   (= (:current-justified-epoch state)
                      (- current-epoch 2)))
            (:current-justified-epoch state)
            %))
        (#(if (and (= (mod bitfield 4)
                      2r11)
                   (= (:current-justified-epoch state)
                      (- current-epoch 1)))
            (:current-justified-epoch state)
            %)))))

(defn- update-justification-bitfield [state bitfield]
  (assoc state :justification-bitfield bitfield))

(defn- rotate-justification [state]
  (-> state
      (update :previous-justified-epoch (:current-justified-epoch state))
      (update :previous-justified-root (:current-justified-root state))))

(defn- update-justification [state new-justified-epoch {:keys [slots-per-epoch] :as system-parameters}]
  (if (not= new-justified-epoch (:current-justified-epoch state))
    (-> state
        (update :current-justified-epoch new-justified-epoch)
        (update :current-justified-root (state/->block-root state (epoch/->start-slot new-justified-epoch slots-per-epoch))))
    state))

(defn- update-finality [state new-finalized-epoch {:keys [slots-per-epoch]}]
  (if (not= new-finalized-epoch (:finalized-epoch state))
    (-> state
        (update :finalized-epoch new-finalized-epoch)
        (update :finalized-root (state/->block-root state (epoch/->start-slot new-finalized-epoch slots-per-epoch))))
    state))

(defn process [state system-parameters]
  (let [{:keys [new-justified-epoch new-justification-bitfield]} (determine-new-justifications state system-parameters)
        new-finalized-epoch (determine-new-finality state new-justification-bitfield system-parameters)]
    (-> (update-justification-bitfield state new-justification-bitfield)
        rotate-justification
        (#(update-justification % new-justified-epoch system-parameters))
        (#(update-finality % new-finalized-epoch system-parameters)))))

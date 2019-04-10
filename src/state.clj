(ns state
  "Contains facilities for dealing with the Eth2.0 state. Note that each forks respective state transition may contain additional semantics around handling the state.")

(defn new []
  {::slot 0})

(defn ->slot [state]
  (::slot state))

(defn slot->epoch
  "Return the epoch number of the given `slot`."
  [slot slots-per-epoch]
  (quot slot slots-per-epoch))

(defn ->current-epoch [state]
  (-> state
      ->slot
      slot->epoch))

(defn ->previous-epoch [state genesis-epoch]
  (max (- (->current-epoch state) 1)
       genesis-epoch))

(defn slot->epoch-start-slot [epoch slots-per-epoch]
  (* epoch slots-per-epoch))

(defn ->validator-registry [state]
  (::validator-registry state))

(defn epoch-committee-count [active-validator-count {:keys [shard-count slots-per-epoch target-committee-size]}]
  (* (max 1
          (min (quot shard-count slots-per-epoch)
               (quot (quot active-validator-count slots-per-epoch)
                     target-committee-size)))
     slots-per-epoch))

(defn- ->committee-count-for-epoch [state epoch system-parameters]
  (-> (->validator-registry state)
      (validator/set->active-indices epoch)
      count
      (->epoch-committee-count system-parameters)))

(defn ->current-epoch-committee-count [state])

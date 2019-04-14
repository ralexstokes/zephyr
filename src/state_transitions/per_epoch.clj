(ns state-transitions.per-epoch
  (:require [state]
            [state-transitions.justification-and-finalization :as justification]
            [state-transitions.crosslinks :as crosslinks]
            [state-transitions.eth1-data :as eth1-data]
            [state-transitions.rewards :as rewards]
            [state-transitions.ejections :as ejections]
            [state-transitions.validator-registry :as validator-registry]
            [state-transitions.slashings :as slashings]
            [state-transitions.exit-queue :as exit-queue]))

(defn- finalize-updates [state system-parameters]
  state)

(defn run-transition [state system-parameters]
  (-> state
      (#(justification/process % system-parameters))
      (#(crosslinks/process % system-parameters))
      (#(eth1-data/process % system-parameters))
      (#(rewards/process % system-parameters))
      (#(ejections/process % system-parameters))
      (#(validator-registry/process % system-parameters))
      (#(slashings/process % system-parameters))
      (#(exit-queue/process % system-parameters))
      (#(finalize-updates % system-parameters))))

(defn transition [state {:keys [genesis-slot slots-per-epoch] :as system-parameters}]
  (if (zero?
       (mod (inc (:slot state)) slots-per-epoch))
    (run-transition state system-parameters)))

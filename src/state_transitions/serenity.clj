(ns state-transitions.serenity
  "This namespace holds the logic for the initial fork of the Eth2.0 system"
  (:require
   [state-transitions.cache :as cache]
   [state-transitions.per-epoch :as epoch]
   [state-transitions.per-slot :as slot]
   [state-transitions.per-block :as block]))

(defn apply-block [state block system-parameters]
  (-> state
      (#(cache/transition % system-parameters))
      (#(epoch/transition % system-parameters))
      slot/transition
      (#(block/transition % block system-parameters))))

(ns .genesis
  "Contains functionality pertaining to the genesis event of Eth2.0."
  (:require [.state :as state]))

(defn state-from-block
  "Returns the genesis state, given the `genesis-block`."
  [genesis-block]
  (let [state (state/new)]
    state))

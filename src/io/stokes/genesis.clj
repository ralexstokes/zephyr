(ns io.stokes.genesis
  "Contains functionality pertaining to the genesis event of Eth2.0."
  (:require [io.stokes.state :as state]))

(defn state-from-block
  "Returns the genesis state, given the `genesis-block`."
  [genesis-block]
  (let [state (state/new)]
    state))

(ns state-machine
  "A `state-machine` manages the state summarizing the underlying blockchain data. In particular, it manages 'forks' in the chain, allowing for a variety of state transition functions to be applied."
  (:require [state]
            [fork-schedule]
            [genesis]))

(defn new
  [fork-schedule]
  {::fork-schedule fork-schedule})

(defn- ->state [state-machine]
  (::state state-machine))

(defn- ->slot [state-machine]
  (let [state (->state state-machine)]
    (.slot state)))

(defn- ->fork-schedule [state-machine]
  (::fork-schedule state-machine))

(defn- ->state-transition
  "Return the state transition function for this `state-machine` based on the provided slot. Use the current slot of the state machine's state if one is not provided."
  ([state-machine]
   (let [slot (->slot state-machine)]
     (->state-transition state-machine slot)))
  ([state-machine slot]
   ((->fork-schedule state-machine) slot)))

(defn apply-block
  "Applies all of the operations in `block` to `state` to yield a new state."
  [state-machine block]
  (let [state-transition (->state-transition state-machine)]
    (update state-machine ::state state-transition block)))

(comment
  (let [genesis-block {}
        fs (fork-schedule/new {})
        sm (state-machine/new (fork-schedule/new {}) genesis-block)]
    sm)
  )

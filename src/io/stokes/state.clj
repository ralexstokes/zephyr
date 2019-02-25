(ns io.stokes.state
  "Contains facilities for dealing with the Eth2.0 state. Note that each forks respective state transition may contain additional semantics around handling the state.")

(defn new []
  {::slot 0})

(defn ->slot [state]
  (::slot state))

(ns state-transitions.per-slot)

(defn transition [state]
  (update state :slot inc))

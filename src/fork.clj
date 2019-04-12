(ns fork)

(defrecord Fork [epoch previous-version current-version])

(defn new [epoch previous-version current-version]
  (->Fork epoch previous-version current-version))

(defn ->version [fork epoch]
  (if (< epoch (.epoch fork))
    (.previous-version fork)
    (.current-version fork)))

(ns fork)

(defrecord Fork [epoch previous-version current-version])

(defn ->version [fork epoch]
  (if (< epoch (.epoch fork))
    (.previous-version fork)
    (.current-version fork)))

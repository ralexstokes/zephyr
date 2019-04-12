(ns slot)

(defn ->epoch [slot slots-per-epoch]
  (quot slot slots-per-epoch))

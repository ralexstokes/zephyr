(ns epoch)

(defn ->start-slot [epoch slots-per-epoch]
  (* epoch slots-per-epoch))

(defn ->delayed-activation-exit-epoch [epoch activation-exit-delay]
  (+ epoch 1 activation-exit-delay))

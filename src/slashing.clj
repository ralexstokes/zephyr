(ns slashing
  (:require [slot]))

(defn is-double-vote? [first second {:keys [slots-per-epoch]}]
  (= (slot/->epoch (.slot first) slots-per-epoch)
     (slot/->epoch (.slot second) slots-per-epoch)))

(defn is-surround-vote? [first second {:keys [slots-per-epoch]}]
  (let [first-source-epoch (.source-epoch first)
        second-source-epoch (.source-epoch second)
        first-target-epoch (slot/->epoch (.slot first) slots-per-epoch)
        second-target-epoch (slot/->epoch (.slot second) slots-per-epoch)]
    (and (< first-source-epoch
            second-source-epoch)
         (< second-target-epoch
            first-target-epoch))))

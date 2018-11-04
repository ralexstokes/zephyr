(ns io.stokes.state
  (:require [io.stokes.hash :as hash]))

(defn new [committees-by-slots previous-crystallization-slot & previous-state]
  (let [state (or previous-state {})]
    (-> state
        (assoc :comittees-by-slots committees-by-slots)
        (assoc :previous-crystallization-slot previous-crystallization-slot))))

(defn slot->committees [crystallized-state slot {:keys [cycle-length]}]
  (let [previous-crystallization-slot (:previous-crystallization-slot crystallized-state)
        first-slot-in-last-cycle (- previous-crystallization-slot cycle-length)]
    (assert (and
             (<= first-slot-in-last-cycle
                 slot)
             (< slot
                (+ first-slot-in-last-cycle
                   (* cycle-length 2)))))
    (nth (:committees-by-slots crystallized-state)
         (- slot
            first-slot-in-last-cycle))))

(defn ->block-hash [active-state block slot]
  (let [first-slot-in-recent-hashes (- (:slot block)
                                       (count (:recent-block-hashes active-state)))]
    (assert (and
             (<= first-slot-in-recent-hashes
                 slot)
             (< slot
                (:slot block))))
    (nth (:recent-block-hashes active-state)
         (- slot
            first-slot-in-recent-hashes))))

(defn record-validator-set-change [crystallized-state index pubkey flag]
  ;; TODO determine serialization strategy for consistent hashing
  ;; e.g. bytes1(flag) bytes3(index) bytes32(pubkey)
  (assoc crystallized-state :validator-set-delta-hash-chain
         (hash/of (:validator-set-delta-hash-chain crystallized-state flag index pubkey))))

(comment
  )

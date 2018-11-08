(ns io.stokes.state
  (:require [io.stokes.hash :as hash]
            [io.stokes.shuffling :as shuffling]
            [io.stokes.bytes :as bytes]
            [io.stokes.validator :as validator]
            [io.stokes.crosslink :as crosslink]
            [integrant.core :as ig]))

(defmethod ig/init-key ::config [_ config]
  (atom (merge config {})))

(defmethod ig/halt-key! ::config [_ _])

(defn create [committees-by-slots previous-crystallization-slot & previous-state]
  (let [state (or previous-state {})]
    (-> state
        (assoc ::comittees-by-slots committees-by-slots)
        (assoc ::previous-crystallization-slot previous-crystallization-slot))))

(defn slot->committees [crystallized-state slot {:keys [cycle-length]}]
  (let [previous-crystallization-slot (::previous-crystallization-slot crystallized-state)
        first-slot-in-last-cycle (- previous-crystallization-slot cycle-length)]
    (assert (and
             (<= first-slot-in-last-cycle
                 slot)
             (< slot
                (+ first-slot-in-last-cycle
                   (* cycle-length 2)))))
    (nth (::committees-by-slots crystallized-state)
         (- slot
            first-slot-in-last-cycle))))

(defn ->block-hash [active-state block slot]
  (let [first-slot-in-recent-hashes (- (:slot block)
                                       (count (::recent-block-hashes active-state)))]
    (assert (and
             (<= first-slot-in-recent-hashes
                 slot)
             (< slot
                (:slot block))))
    (nth (::recent-block-hashes active-state)
         (- slot
            first-slot-in-recent-hashes))))

(defn record-validator-set-change [crystallized-state index pubkey flag]
  ;; TODO determine serialization strategy for consistent hashing
  ;; e.g. bytes1(flag) bytes3(index) bytes32(pubkey)
  (assoc crystallized-state ::validator-set-delta-hash-chain
         (hash/value (::validator-set-delta-hash-chain crystallized-state flag index pubkey))))

;; TODO reconcile w/ new
(defn new-crystallized-state [validators crosslinks genesis-shuffling initial-fork-version]
  {::validator-set-change-slot 0
   ::validators validators
   ::crosslinks crosslinks
   ::previous-crystallization-slot 0
   ::last-finalized-slot 0
   ::last-justified-slot 0
   ::justified-streak 0
   ::committees-by-slots (concat genesis-shuffling genesis-shuffling)
   ::deposits-penalized-in-period []
   ::validator-set-delta-hash-chain (bytes/empty-array 32)
   ::pre-fork-version initial-fork-version
   ::post-fork-version initial-fork-version
   ::fork-slot-number 0})

(defn new-active-state [recent-block-hashes]
  {::pending-attestations []
   ::pending-specials []
   ::recent-block-hashes recent-block-hashes
   ::randao-mix (bytes/empty-array 32)})

(defn genesis
  "parses the initial set of validator logs to prepare the beacon chain genesis state"
  [initial-validator-logs {:keys [shard-count initial-fork-version cycle-length] :as constants}]
  (let [validators (validator/create-set initial-validator-logs)
        genesis-shuffling (validator/new-shuffling-to-slots-and-committees (bytes/empty-array 32) validators 0 constants)
        crosslinks (repeat shard-count (crosslink/new))
        crystallized-state (new-crystallized-state validators crosslinks genesis-shuffling initial-fork-version)
        recent-block-hashes (repeat (* 2 cycle-length) (bytes/empty-array 32))
        active-state (new-active-state recent-block-hashes)]
    [crystallized-state active-state]))

(comment
  )

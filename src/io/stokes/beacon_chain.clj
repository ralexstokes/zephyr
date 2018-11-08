(ns io.stokes.beacon-chain
  (:gen-class)
  (:require
   [clojure.pprint :as pp]
   [io.stokes.validator :as validator]
   [io.stokes.shuffling :as shuffling]
   [io.stokes.math :as math]))

(def base-constants
  {:shard-count (math/exp 2 10)
   :deposit-size (math/exp 2 5)
   :min-online-deposit-size (math/exp 2 4)
   :gwei-per-eth (math/exp 10 9)
   :min-committee-size (math/exp 2 7)
   :genesis-time (java.util.Date.)
   :slot-duration (math/exp 2 4)
   :cycle-length (math/exp 2 6)
   :min-validator-set-change-interval (math/exp 2 8)
   :randao-slots-per-layer (math/exp 2 12)
   :sqrt-e-drop-time (math/exp 2 16)
   :withdrawal-period (math/exp 2 19)
   :base-reward-quotient (math/exp 2 15)
   :max-validator-churn-quotient (math/exp 2 5)
   :logout-message "LOGOUT"
   :initial-fork-version 0

   :validator-count 312500})

(def small-constants
  (-> base-constants
      (assoc :shard-count 6)
      (assoc :cycle-length 6)
      (assoc :validator-count 16)))

(defn- constants [base-constants & args]
  (let [max-committees-per-slot (quot (:shard-count base-constants)
                                      (:cycle-length base-constants))]
    (-> base-constants
        (assoc :max-committees-per-slot max-committees-per-slot))))

(defn- initialize-validators [count]
  (for [id (range count)]
    (validator/create id {})))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [constants (apply constants base-constants args)
        validators (initialize-validators (:validator-count constants))]
    (pp/pprint (shuffling/of-validators-to-slots-and-committees :some-seed validators 0 constants))))

(comment
  (def constants (constants small-constants))
  constants
  (:min-committee-size constants)

  (def validators (initialize-validators (:validator-count constants)))
  validators

  (def seed (byte-array (map byte (range 1 33))))
  seed

  (def shuffling
    (validator/new-shuffling-to-slots-and-committees seed validators 0 constants))
  shuffling
  )

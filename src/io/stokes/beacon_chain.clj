(ns io.stokes.beacon-chain
  (:gen-class)
  (:require
   [clojure.pprint :as pp]
   [io.stokes.validator :as validator]
   [io.stokes.shuffling :as shuffling]))

(defn exp [base power]
  (int (Math/pow base power)))

(def base-constants
  {:shard-count (exp 2 10)
   :deposit-size (exp 2 5)
   :min-online-deposit-size (exp 2 4)
   :gwei-per-eth (exp 10 9)
   :min-committee-size (exp 2 7)
   :genesis-time (java.util.Date.)
   :slot-duration (exp 2 4)
   :cycle-length (exp 2 6)
   :min-validator-set-change-interval (exp 2 8)
   :randao-slots-per-layer (exp 2 12)
   :sqrt-e-drop-time (exp 2 16)
   :withdrawal-period (exp 2 19)
   :base-reward-quotient (exp 2 15)
   :max-validator-churn-quotient (exp 2 5)
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
    (validator/new id)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [constants (apply constants base-constants args)
        validators (initialize-validators (:validator-count constants))]
    (pp/pprint (shuffling/of-validators-to-slots-and-committees :some-seed validators 0 constants))))

(comment
  (def constants (constants small-constants))
  constants

  (def validators (initialize-validators (:validator-count constants)))
  validators

  (shuffling/of-validators-to-slots-and-committees :some-seed validators 0 constants)
  )

(ns user
  (:require
   [integrant.repl :refer [clear go halt prep init reset reset-all]]
   [integrant.core :as ig]
   [node :as node]))

(def small-constants? true)

(defn- small-constants [base-constants]
  (-> base-constants
      (assoc :shard-count 6)
      (assoc :cycle-length 6)
      (assoc :validator-count 16)))

(defn select-config []
  (let [config (-> (node/default-config)
                   (update-in [:node/config :constants] node/constants))]
    (if small-constants?
      (update-in config [:node/config :constants] small-constants)
      config)))

(integrant.repl/set-prep! select-config)

(comment
  (def constants (constants small-constants))
  constants
  (:min-committee-size constants)

  (def some-seed (byte-array (map byte (range 1 33))))

  (def validators (initialize-validators (:validator-count constants)))
  validators

  (def shuffling (validator/new-shuffling-to-slots-and-committees validators some-seed 0 constants))
  shuffling

  (let [constants (apply constants base-constants args)
        validators (initialize-validators (:validator-count constants))]
    (pp/pprint (validator/new-shuffling-to-slots-and-committees validators some-seed 0 constants)))
  )

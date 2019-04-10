(ns node
  (:gen-class)
  (:require
   [integrant.core :as ig]
   [clojure.tools.cli :as cli]
   [clojure.java.io :as io]
   [math]))

(def default-config-filename (io/resource "config.edn"))

(defn expand-exp [[base pow]]
  (math/exp base pow))

(defn- build-config [filename]
  (->> filename
       slurp
       (ig/read-string
        {:readers {'math/exp expand-exp}})))

(defn default-config []
  (build-config default-config-filename))

(defn constants [base-constants & args]
  (let [max-committees-per-slot (quot (:shard-count base-constants)
                                      (:cycle-length base-constants))]
    (-> base-constants
        (assoc :max-committees-per-slot max-committees-per-slot))))

(defn- wait-for-shutdown-signal []
  ;; TODO sort out control flow
  )

(defn- launch [config]
  (let [system (ig/init config)]
    (wait-for-shutdown-signal)
    (ig/halt! system)))

(def cli-options [])

(defn ->config-filename [opts]
  (-> opts
      :arguments
      first
      (or default-config-filename)))

(defn -main
  "Launches an instance of the beacon node"
  [& args]
  (let [opts (cli/parse-opts args cli-options)
        filename (->config-filename opts)
        config (build-config filename)]
    (ig/load-namespaces config)
    (launch config)))

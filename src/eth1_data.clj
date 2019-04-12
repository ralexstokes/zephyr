(ns eth1-data
  (:refer-clojure :exclude [empty])
  (:require [hash]))

(defrecord Eth1Data [deposit-root deposit-count block-hash])

(defn empty []
  (map->Eth1Data
   {:deposit-root hash/zero
    :deposit-count 0
    :block-hash hash/zero}))

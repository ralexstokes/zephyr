(ns merkle
  "borrowing the merkle implementation in github.com/ethereum/trinity"
  (:require [bytes]
            [hash]))

(defn- ->branch-indices [node-index depth]
  (take depth (iterate #(quot % 2) node-index)))

(defn- compute-parent-hash [left right]
  (hash/value (bytes/join left right)))

(defn- compute-proof-node [accumulator [proof-elem orderer]]
  (apply compute-parent-hash (orderer [accumulator proof-elem])))

(defn- compute-root [value proof depth index]
  (let [branch-indices (->branch-indices index (count proof))
        node-orderers (map #(if (even? %) identity reverse) branch-indices)]
    (reduce compute-proof-node value (map vector proof node-orderers))))

(defn branch-valid?
  "from spec: verify_merkle_branch"
  [leaf proof depth index root]
  (= root
     (compute-root leaf proof depth index)))

(ns ssz
  (:require [ssz.serialize :as ser]
            [ssz.deserialize :as des]))

(def serialize ser/->bytes)

(def deserialize des/from-bytes)

(def hash-tree-root ser/hash-tree-root)

(def signing-root ser/signing-root)

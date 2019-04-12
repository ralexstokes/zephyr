(ns block
  (:require [hash]
            [bls]
            [ssz]
            [ssz.schemas :as schemas]))

(defrecord BeaconBlockBody [randao-reveal eth1-data proposer-slashings attester-slashings attestations deposits voluntary-exits transfers])

(defrecord BeaconBlock [slot previous-block-root state-root body signature])

(defrecord BeaconBlockHeader [slot previous-block-root state-root block-body-root signature])

(defn ->temporary-block-header [block]
  (map->BeaconBlockHeader
   {:slot (.slot block)
    :previous-block-root (.previous-block-root block)
    :state-root hash/zero
    :block-body-root (ssz/hash-tree-root (.body block) schemas/beacon-block-body)
    :signature bls/empty-signature}))

(comment
  (let [block-body (->BeaconBlockBody "reveal" "eth1-data" [] [] [] [] [] [])
        block (->BeaconBlock 200 "foo" "bar" block-body "sig")
        header (->temporary-block-header block)]
    header)
  )

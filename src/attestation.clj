(ns attestation)

(defrecord Data [slot beacon-block-root source-epoch source-root target-root shard previous-crosslink crosslink-data-root])

(defrecord Attestation [aggregation-bitfield data custody-bitfield aggregate-signature])

(defrecord IndexedAttestation [custody-bit-0-indices custody-bit-1-indices data aggregate-signature])

(defrecord AttestationDataAndCustodyBit [data custody-bit])

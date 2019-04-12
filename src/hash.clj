(ns hash
  "Contains functionality for producing (canonical) hashes of both byte strings and generic data"
  (:require [bytes])
  (:import
   org.bouncycastle.crypto.digests.SHA256Digest))

(def zero (bytes/zeros 32))

(defn- sha-256
  "NOTE: modified from: https://github.com/jamesleonis/pen-of-midas/blob/8c8f56be57ab2e24c7607113a21a32215033144c/src/pen_of_midas/core.clj#L63"
  [^bytes data]
  (let [engine (doto (SHA256Digest.) (.update data 0 (count data)))
        buffer (byte-array (.getDigestSize engine))]
    (.doFinal engine buffer 0)
    buffer))

(defn value
  "Returns the canonical hash of the `input` bytes."
  [^bytes input]
  (sha-256 input))

(defn value-as-hex
  "Returns the `value` encoded as a hex string."
  [^bytes input]
  (bytes/->hex (value input)))

(defn serialize
  "Serializes the given `args` in the canonical way."
  [& args]
  (apply str args))

(defn of
  "Returns the canonical hash of the provided `args`, serializing the data as required."
  [& args]
  (-> (apply serialize args)
      bytes/->array
      value))

(defn as-hex
  "Returns the canonical hash computed by `of`, encoded as a hex string."
  [& args]
  (-> (apply of args)
      bytes/->hex))

(comment
  )

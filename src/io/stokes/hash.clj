(ns io.stokes.hash
  (:require
   [buddy.core.hash :as hash]
   [io.stokes.bytes :as bytes]
   [io.stokes.simple-serialize :as ssz]))

(defn value
  "input is an array of bytes; returns the canonical hash"
  [^bytes input]
  (bytes/slice (hash/blake2b-512 input) 0 32))

(defn of
  "this function takes any number of arguments and returns the appropriate hash of their `ssz` serialized values."
  [& args]
  (let [serialized-args (apply ssz/serialize args)]
    (value serialized-args)))

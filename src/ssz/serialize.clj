(ns ssz.serialize
  (:require [hash]
            [bytes]))

(defn ->bytes [object schema]
  ;; do the proper serialization
  ;; as a temporary stub, just hash the object
  (-> object
      pr-str
      .getBytes
      hash/value))

(defn hash-tree-root [object schema]
  ;; TODO make the proper hash-tree-root
  (->bytes object schema))

(defn signing-root [object schema]
  ;; TODO make proper signing-root
  (->bytes object schema))

(comment
  (defrecord Foo [a b])

  (let [object (Foo. 12 13)]
    (-> (->bytes object 'foo)
        bytes/->hex))
  )

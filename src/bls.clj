(ns bls
  (:require [bytes]
            [fork]))

(def empty-signature (bytes/zeros 96))

(defn- ->domain [fork epoch domain-type]
  (bytes/->int
   (bytes/join
    (fork/->version fork epoch)
    (bytes/from-int4 domain-type))))

(defn multiple-valid? [pubkeys message-hashes signature fork slot domain-type]
  ;; TODO
  true)

(defn valid-signature? [& args]
  ;; TODO
  true)

(defn aggregate-pubkeys [& pubkeys]
  ;; TODO
  (apply str pubkeys))

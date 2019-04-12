(ns shuffling
  (:require [validator]
            [bytes]
            [hash]))

(defn get-split-offset [list-size chunks index]
  (quot (* list-size index) chunks))

(defn- -permuted-index
  "The inner loop of `permuted-index`"
  ([index list-size seed shuffle-round-count]
   (-permuted-index 0 index list-size seed shuffle-round-count))
  ([round index list-size seed shuffle-round-count]
   (if (= round shuffle-round-count)
     index
     (let [pivot (mod (bytes/->int
                       (hash/value
                        (bytes/join seed (byte round))))
                      list-size)
           flip (mod (- pivot index)
                     list-size)
           position (max index flip)
           source (hash/value
                   (bytes/join seed (byte round)
                               (bytes/from-int4
                                (quot position 256))))
           byte (aget source (quot (mod position 256) 8))
           bit (not (zero? (mod (bit-shift-right byte (mod position 8)) 2)))
           index (if bit
                   flip
                   index)]
       (recur (inc round) index list-size seed shuffle-round-count)))))

(defn permuted-index [index list-size seed shuffle-round-count]
  (if (>= index list-size)
    (throw
     (ex-info
      "`index` argument to `permuted-index` must be less than the `list-size` argument."
      {:index index
       :list-size list-size
       :seed seed})))
  (if (> list-size (Math/pow 2 40))
    (throw (ex-info "`list-size` must be less than 2**40"
                    {:index index
                     :list-size list-size
                     :seed seed})))
  (-permuted-index index list-size seed shuffle-round-count))

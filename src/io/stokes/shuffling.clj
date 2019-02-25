(ns io.stokes.shuffling
  (:require [io.stokes.validator :as validator]
            [io.stokes.bytes :as bytes]
            [io.stokes.hash :as hash]))

(defn- split-into-pieces
  "splits up the `seq` into `split-count` pieces"
  [seq split-count]
  (let [len (count seq)]
    (for [i (range split-count)]
      (let [lower (quot (* len i) split-count)
            upper (quot (* len (+ i 1)) split-count)]
        (->> seq
             (drop lower)
             (take (- upper lower)))))))

(defn- -permuted-index
  "The inner loop of `permuted-index`"
  ([index list-size seed shuffle-round-count]
   (-permuted-index 0 index list-size seed shuffle-round-count))
  ([round index list-size seed shuffle-round-count]
   (if (= round shuffle-round-count)
     index
     (let [pivot (mod (bytes/->int
                       (hash/slice
                        (hash/value
                         (bytes/join seed (byte round)))
                        0 8))
                      list-size)
           flip (mod (- pivot index)
                     list-size)
           position (max index flip)
           source (hash/value
                   (bytes/join seed (byte round)
                               (bytes/int->bytes4
                                (quot position 256))))
           byte (aget source (quot (mod position 256) 8))
           bit (not (zero? (mod (bit-shift-right (mod position 8)) 2)))
           index (if bit
                   flip
                   index)]
       (recur (inc round) index list-size seed shuffle-round-count)))))

(defn- permuted-index [index list-size seed shuffle-round-count]
  (if (>= index list-size)
    (throw (ex-info "`index` argument to `permuted-index` must be less than the `list-size` argument."
                    {:index index
                     :list-size list-size
                     :seed seed})))
  (if (> list-size (Math/pow 2 40))
    (throw (ex-info "`list-size` must be less than 2**40"
                    {:index index
                     :list-size list-size
                     :seed seed})))
  (-permuted-index index list-size seed shuffle-round-count))

(defn from-seed
  "Shuffle the active `validators` and split into crosslink committees.
   `seed` is a 32 byte value.
   Returns a seq of committees (each a seq of validator indices in that committee)."
  [^bytes seed validators epoch {:keys [shuffle-round-count] :as system-parameters}]
  (let [active-indices (validator/set->active-indices validators epoch)
        count (count active-indices)
        shuffled-keys (set (map #(permuted-index %1 count seed shuffle-rount-count) (range count)))
        shuffled-indices (keep-indexed #(if (shuffled-keys %1) %2) active-indices)
        count-pieces (state/epoch-committee-count count system-parameters)]
    (split-into-pieces shuffled-indices count-pieces)))

(comment
  (split-into-pieces (range 15) 3)
  (def seed (byte-array (map byte (range 1 33))))

  seed

  (from-seed seed (range 10) 3 {})

  )

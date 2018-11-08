(ns io.stokes.shuffling
  (:require [io.stokes.validator :as validator]
            [io.stokes.math :as math]
            [io.stokes.hash :as hash]
            [io.stokes.bytes :as bytes]))

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

(defn swap
  "replaces the element in `seq` at index `a` with the element in `seq` at index `b`and vice-versa"
  [seq a b]
  (assoc seq b (seq a) a (seq b)))

(defn- -shuffle
  "Inner loop to match the `shuffle` function from the spec"
  [rand-max values-count range-upper-bound rand-bytes output source index]
  (let [new-source (hash/value source)]
    (loop [position 0
           output output
           index index]
      (if (>= position range-upper-bound)
        [output new-source index]
        (let [remaining (- values-count index)]
          (if (= remaining 1)
            [output new-source index]
            (let [next-chunk (bytes/slice new-source position (+ position rand-bytes))
                  sample-from-source (bytes/->int next-chunk)
                  sample-max (- rand-max (mod rand-max remaining))]
              (if (< sample-from-source sample-max)
                (let [replacement-position (+ (mod sample-from-source remaining) index)]
                  ;; TODO hoist this recur
                  (recur (+ position rand-bytes)
                         (swap output index replacement-position)
                         (inc index)))
                (recur (+ position rand-bytes)
                       output
                       index)))))))))

(defn- shuffle-with-seed
  "Returns the shuffled `seq` with seed as entropy. `seed` is an array of bytes of length 32."
  [seq ^bytes seed]
  (let [values-count (count seq)
        rand-bytes 3
        rand-max (- (math/exp 2 (* rand-bytes 8)) 1)
        range-upper-bound (- 32 (mod 32 rand-bytes))]
    (assert (< values-count rand-max))
    (loop [output (into [] seq)
           source seed
           index 0]
      (if (>= index (- values-count 1))
        output
        (let [[new-output new-source new-index]
              (-shuffle rand-max values-count range-upper-bound rand-bytes output source index)]
          (recur new-output new-source new-index))))))

(defn- clamp [min max value]
  (cond
    (<= value min) min
    (>= value max) max
    :else value))

(defn- calculate-committes-per-slot [shard-count cycle-length active-validator-count min-committee-size]
  (clamp 1 (quot shard-count cycle-length)
         (quot
          (quot active-validator-count
                cycle-length)
          (+ (* min-committee-size 2) 1))))

(def allocate-validators-to-slots split-into-pieces)

(defn- allocate-validators-in-each-slot-to-committees [validators-by-slot committees-per-slot current-shard shard-count]
  (map-indexed
   (fn [slot-offset validators]
     (let [validators-by-shard (split-into-pieces validators committees-per-slot)
           starting-shard-for-this-committee (+ current-shard (* slot-offset
                                                                 committees-per-slot))]
       (map-indexed
        (fn [shard-offset validators]
          {::shard (mod (+ starting-shard-for-this-committee shard-offset)
                        shard-count)
           ::committee validators})
        validators-by-shard)))
   validators-by-slot))

(defn of-validators-to-slots-and-committees [seed validators current-shard {:keys [shard-count
                                                                                   cycle-length
                                                                                   min-committee-size]}]
  (let [active-validators (filter (validator/status-is :active) validators)
        committees-per-slot (calculate-committes-per-slot
                             shard-count
                             cycle-length
                             (count active-validators)
                             min-committee-size)
        shuffled-validators (shuffle-with-seed active-validators seed)
        validators-by-slot (split-into-pieces shuffled-validators cycle-length)]
    (-> active-validators
        (shuffle-with-seed seed)
        (allocate-validators-to-slots cycle-length)
        (allocate-validators-in-each-slot-to-committees committees-per-slot current-shard shard-count))))

(comment
  (split-into-pieces (range 15) 3)
  )

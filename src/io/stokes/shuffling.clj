(ns io.stokes.shuffling
  (:require [io.stokes.validator :as validator]))

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

(defn- shuffle-with-seed
  "Returns the shuffled `seq` with seed as entropy."
  [seq seed]
  ;; TODO implement shuffle from spec
  (shuffle seq))

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

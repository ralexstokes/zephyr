(ns validator
  "Contains logic for managing validators.")

(defn new [far-future-slot]
  {::activation-epoch far-future-slot
   ::exit-epoch far-future-slot})

(defn ->activation-epoch [validator]
  (::activation-epoch validator))

(defn ->exit-epoch [validator]
  (::exit-epoch validator))

(defn is-active?
  "Is the `validator` active at the given `epoch`?"
  [validator epoch]
  (and (<= (->activation-epoch validator)
           epoch)
       (< epoch
          (->exit-epoch validator))))

(defn set->active-indices [validators epoch]
  (keep-indexed #(if (is-active? %2 epoch) %1) validators))

(comment
  (let [some-epoch 20
        some-activation (- some-epoch 3)
        far-future-epoch 200000
        validators (repeat 3 (validator/new far-future-epoch))
        active-validators (map-indexed (fn [index validator]
                                         (if (even? index)
                                           (assoc validator ::activation-epoch some-activation)
                                           validator)) validators)]
    (set->active-indices active-validators some-epoch))
  )

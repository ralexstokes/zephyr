(ns io.stokes.validator)

(defn new [id]
  {::status :active
   ::id id})

(defn status-is [status]
  (fn [validator]
    (= (::status validator)
       status)))

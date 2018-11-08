(ns io.stokes.crosslink
  (:require [io.stokes.bytes :as bytes]))

(defn new []
  {::recently-changed false
   ::slot 0
   ::hash (bytes/empty-array 32)})

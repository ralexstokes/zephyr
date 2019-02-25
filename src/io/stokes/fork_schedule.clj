(ns io.stokes.fork-schedule
  "A `fork-schedule` is a mapping of slot number to the appropriate state transition function."
  (:require [io.stokes.state-transitions.serenity :as serenity]))

(defn new [config]
  #(serenity/apply-block))

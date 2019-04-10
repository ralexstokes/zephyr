(ns fork-schedule
  "A `fork-schedule` is a mapping of slot number to the appropriate state transition function."
  (:require [state-transitions.serenity :as serenity]))

(defn new [config]
  #(serenity/apply-block))

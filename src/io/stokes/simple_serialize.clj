(ns io.stokes.simple-serialize)

(defn serialize [& args]
  ;; TODO https://github.com/ztellman/gloss/wiki/Introduction
  (apply str args))

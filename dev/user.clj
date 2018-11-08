(ns user
  (:require
   [integrant.repl :refer [clear go halt prep init reset reset-all]]
   [integrant.core :as ig]
   ))

(integrant.repl/set-prep! (constantly {:io.stokes.state/config {:example? true}}))

(def a (atom nil))

(defmethod ig/init-key ::foo [_ {:keys [example?]}]
  (swap! a (constantly example?)))

(defmethod ig/halt-key! ::foo [_ blah]
  (reset! a nil))

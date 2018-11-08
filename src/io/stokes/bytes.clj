(ns io.stokes.bytes
  (:require
   [buddy.core.bytes :as buddy-bytes]
   [buddy.core.codecs :as codecs]))

(defn empty-array [size]
  (byte-array size (repeat 0x00)))

(def slice buddy-bytes/slice)

(defn ->int
  "https://gist.github.com/pingles/1235344#gistcomment-2103894"
  [data]
  (reduce bit-or (map-indexed (fn [i x] (bit-shift-left (bit-and x 0x0FF) (* 8 (- (count data) i 1)))) data)))

(def ->hex codecs/bytes->hex)

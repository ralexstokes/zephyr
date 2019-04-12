(ns bitfield)

(defn new []
  (byte-array [(byte 0x0)]))

(defn- get-bitfield-bit [bitfield index]
  (-> (aget bitfield (quot index 8))
      (unsigned-bit-shift-right (mod index 8))
      (mod 2)))

(defn has-bit-set? [bitfield index]
  (not (zero? (get-bitfield-bit bitfield index))))

(defn- bit-count->byte-count [n]
  (quot (+ n 7) 8))

(defn- has-valid-number-of-bytes? [bitfield set-size]
  (= (count bitfield)
     (bit-count->byte-count set-size)))

(defn- has-empty-extra-high-bits? [bitfield set-size]
  (every? true? (map (complement #(has-bit-set? bitfield %)) (range set-size (* (count bitfield) 8)))))

(defn valid? [bitfield set-size]
  (and
   (has-valid-number-of-bytes? bitfield set-size)
   (has-empty-extra-high-bits? bitfield set-size)))

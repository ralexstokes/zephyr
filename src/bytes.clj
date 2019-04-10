(ns .bytes
  "Utilities for dealing with bytes and data that can be converted into bytes.")

(defn join
  "Joins a seq of byte arrays into one byte array."
  [& arrays]
  (byte-array (apply concat arrays)))

(defn ->array [input]
  (cond
    (string? input) (.getBytes input "UTF-8")
    :else (throw
           (ex-info "cannot convert input to byte array; please extend .bytes/to-array for your type"
                    {:input input
                     :type (type input)}))))

(def ^:private hex [\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \a \b \c \d \e \f])

(defn ->hex [bytes]
  "Modified from: https://stackoverflow.com/a/15627016"
  (letfn [(hexify-byte [b]
            (let [v (bit-and b 0xFF)]
              [(hex (bit-shift-right v 4)) (hex (bit-and v 0x0F))]))]
    (apply str (mapcat hexify-byte bytes))))

(defn ->int
  [data]
  (reduce bit-or (map-indexed (fn [i x] (bit-shift-left (bit-and x 0x0FF) (* 8 (- (count data) i 1)))) data)))

(comment
  (let [input "hi"
        bytes (->array input)
        output (->hex bytes)]
    [bytes (str "0x" output) (String. bytes "UTF-8")])
  )

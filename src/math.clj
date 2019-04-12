(ns math)

(defn exp [base power]
  (int (Math/pow base power)))

(defn integer-squareroot [n]
  (assert (>= n 0))
  (loop [x n
         y (quot (+ x 1) 2)]
    (if (< y x)
      (recur y (quot (+ y (quot n y)) 2))
      x)))

(comment
  (integer-squareroot 18)
  )

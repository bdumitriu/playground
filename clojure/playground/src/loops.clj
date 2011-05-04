(defn sum-down-from [initial-x]
  (loop [sum 0, x initial-x]
    (if (pos? x)
      (recur (+ x sum) (dec x))
      sum)))

(defn double-looped-sum [initial-x initial-y]
  (loop [sum 0, x initial-x]
    (if (pos? x)
      (let [inner-sum (loop [s sum, y initial-y] (if (pos? y) (recur (+ y s) (dec y)) s))]
        (recur (+ x inner-sum) (dec x)))
      sum)))

(defn x [] (double-looped-sum 9 8))

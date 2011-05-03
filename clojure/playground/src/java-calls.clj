(defn method-call [key]
  (let [map (java.util.HashMap. {"foo" 42 "bar" 9 "baz" "quux"})]
    (.get map key)))

(defn set-field []
  (let [point (java.awt.Point. 45 67)]
    (set! (.x point) 15)
    (str point)))

(defn multiple-method-calls-on-same-field []
  (let [map (java.util.HashMap.)]
    (doto map
      (.put "HOME" "/home/bdumitriu")
      (.put "SRC" "src")
      (.put "BIN" "bin"))
    (str map)))
